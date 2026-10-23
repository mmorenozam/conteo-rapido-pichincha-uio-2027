/*
 * Extraction of the CNE 2025 elecciones generales (first round, 9 Feb 2025) results by
 * junta receptora del voto, dignidad Presidente y Vicepresidente, provincia Pichincha.
 *
 * Where it runs: in the browser console of a page opened at
 *   https://app01.cne.gob.ec/Resultados2025/
 * The 2025 site is the same application as 2023 (same endpoints under /resultados2025/),
 * so this is scraper_cne2023.js with the base path, the dignidad and the file names
 * changed. It uses the page's own endpoints and anti-forgery token. The consult button
 * has reCAPTCHA disabled ("Consulta(url, 'False')"); this script does not touch any captcha.
 *
 * Endpoints (all POST, header __RequestVerificationToken from the page):
 *   ResultadosSeccionales/CatalogoParroquiaJson  form: intCanton, intCircunscripcion=-1, intDignidad
 *   Resultados/CatalogoZonaJson                  form: intParroquia, intCircunscripcion=0
 *   ResultadosSeccionales/CatalogoJuntaJson      form: intZona, intParroquia, intCircunscripcion=0
 *   Resultados/ConsultaInformacionResultados     json: {objConsulta:{Dignidad, Provincia, Canton, Parroquia, Zona, Junta, ...}}
 *
 * Codes: dignidad 11 = Presidente; provincia 17 = Pichincha;
 *   cantones 60 Quito, 65 Cayambe, 70 Mejía, 75 Pedro Moncayo, 80 Rumiñahui,
 *   770 S. M. de los Bancos, 895 P. V. Maldonado, 896 Puerto Quito.
 *
 * Politeness: one request at a time, 700 ms pause before each, up to 5 retries with
 * growing back-off, 30 s timeout per request. A supervisor resumes the run after an error.
 * If the site answers with its firewall check page, the run stops (it is never bypassed).
 *
 * Checkpoints: IndexedDB 'scr_cne2025' per parroquia, localStorage 'scr25_done'; one CSV
 * per canton is downloaded: cne2025_d11_c<canton>_CANTON.csv.
 *
 * Known catalog issue: the parroquia catalog lists JUAN MONTALVO (Cayambe) under code 5495,
 * which returns no zonas or juntas. Its 18 juntas are under code 4765 (the code in the SPSS
 * file and the distributivo). SCR.EXTRA scrapes it after the main run and downloads
 * cne2025_d11_c65_p4765_JUAN-MONTALVO.csv.
 *
 * Run (20 Oct 2026, 07:32 to 10:30 UTC): 7,061 juntas, 0 failed, no firewall block;
 * all 8 cantons match Primera-Vuelta.sav exactly.
 *
 * Use: paste this file in the console, then
 *   SCR.sup = SCR.supervise().then(() => SCR.blocked ? null : SCR.runExtra());   // start or resume
 *   SCR.progress();              // status
 */
window.SCR = window.SCR || {};
SCR.delay = 700;
SCR.nreq = 0; SCR.errors = []; SCR.log = [];
SCR.sleep = ms => new Promise(r => setTimeout(r, ms));
SCR.tok = () => $('input[name="__RequestVerificationToken"]').val();

SCR.post = async (url, data, json) => {
  if (SCR.blocked) throw new Error('BLOCKED');
  for (let a = 0; a < 5; a++) {
    try {
      await SCR.sleep(SCR.delay);
      SCR.nreq++;
      const headers = { '__RequestVerificationToken': SCR.tok(), 'X-Requested-With': 'XMLHttpRequest',
        'Content-Type': json ? 'application/json; charset=utf-8' : 'application/x-www-form-urlencoded; charset=UTF-8' };
      const ac = new AbortController(); const to = setTimeout(() => ac.abort(), 30000);
      let r, txt; try { r = await fetch(url, { method: 'POST', headers, signal: ac.signal, body: json ? JSON.stringify(data) : new URLSearchParams(data) }); txt = await r.text(); } finally { clearTimeout(to); }
      if (/_Incapsula_Resource|Incapsula incident/.test(txt)) { SCR.blocked = new Date().toISOString(); throw new Error('BLOCKED'); }
      if (r.status !== 200) throw new Error('HTTP ' + r.status);
      return JSON.parse(txt);
    } catch (e) {
      SCR.errors.push({ url, err: String(e), a, t: new Date().toISOString() });
      if (SCR.blocked) throw e;
      await SCR.sleep(3000 * (a + 1) ** 2);
    }
  }
  throw new Error('failed ' + url);
};

// --- parse one junta response -------------------------------------------------------
SCR.txt = s => (s || '').replace(/<[^>]+>/g, ' ').replace(/&#(\d+);/g, (m, c) => String.fromCharCode(c)).replace(/&nbsp;/g, ' ').replace(/\s+/g, ' ');
SCR.num = (re, s) => { const m = s.match(re); return m ? Number(m[1].replace(/\./g, '').replace(',', '.')) : null; };
SCR.parse = (j, meta) => {
  const p = SCR.txt(j.VistaParcialProcesamiento);
  const head = { ...meta,
    blancos: SCR.num(/Blancos ([\d\.]+)/, p), nulos: SCR.num(/Nulos ([\d\.]+)/, p),
    sufragantes: SCR.num(/Total Sufragantes ([\d\.]+)/, p), ausentismo: SCR.num(/Ausentismo ([\d\.]+)/, p),
    actas_validas: SCR.num(/Actas v.lidas \((\d+)\)/, p) ?? SCR.num(/Actas validadas \((\d+)\)/, p), actas_novedad: SCR.num(/Actas con novedad \((\d+)\)/, p),
    actas_por_procesar: SCR.num(/Actas por procesar \((\d+)\)/, SCR.txt(JSON.stringify(j.ModelProcesamientoConsulta))) };
  return (j.datos || []).map(d => ({ ...head, op_codigo: d.intCodPartido, op_nombre: d.strNomPartido,
    op_lista: d.strLisPartido, op_orden: d.strOrdPartido, candidato: d.strNomCandidato, votos: d.intVotos }));
};

// --- CSV, storage and downloads -------------------------------------------------------
SCR.COLS = ['dignidad','provincia','canton','parroquia_codigo','parroquia','zona_codigo','zona','junta_id','junta',
  'sufragantes','ausentismo','blancos','nulos','actas_validas','actas_novedad','actas_por_procesar',
  'op_codigo','op_lista','op_orden','op_nombre','candidato','votos'];
SCR.csv = rows => { const q = v => v == null ? '' : (typeof v === 'string' && /[",\n;]/.test(v) ? '"' + v.replace(/"/g, '""') + '"' : String(v));
  return [SCR.COLS.join(','), ...rows.map(r => SCR.COLS.map(c => q(r[c])).join(','))].join('\n') + '\n'; };
SCR.slug = s => s.normalize('NFD').replace(/[̀-ͯ]/g, '').replace(/[^A-Za-z0-9]+/g, '-').replace(/^-|-$/g, '').toUpperCase();
SCR.fname = (dig, canton, p, name) => `cne2025_d${dig}_c${canton}_p${p}_${SCR.slug(name)}.csv`;
SCR.download = (name, text) => { const a = document.createElement('a'); a.href = URL.createObjectURL(new Blob([text], { type: 'text/csv;charset=utf-8' }));
  a.download = name; document.body.appendChild(a); a.click(); setTimeout(() => { URL.revokeObjectURL(a.href); a.remove(); }, 5000); };
SCR.ls = { get: () => { try { return JSON.parse(localStorage.getItem('scr25_done') || '{}'); } catch (e) { return {}; } },
           set: o => { try { localStorage.setItem('scr25_done', JSON.stringify(o)); } catch (e) {} } };
SCR.idb = () => new Promise((res, rej) => { const r = indexedDB.open('scr_cne2025', 1); r.onupgradeneeded = () => r.result.createObjectStore('files'); r.onsuccess = () => res(r.result); r.onerror = () => rej(r.error); });
SCR.idbPut = async (k, v) => { const db = await SCR.idb(); return new Promise((res, rej) => { const tx = db.transaction('files', 'readwrite'); tx.objectStore('files').put(v, k); tx.oncomplete = () => res(true); tx.onerror = () => rej(tx.error); }); };
SCR.idbAll = async () => { const db = await SCR.idb(); return new Promise((res, rej) => { const out = {}; const c = db.transaction('files', 'readonly').objectStore('files').openCursor();
  c.onsuccess = e => { const cur = e.target.result; if (cur) { out[cur.key] = cur.value; cur.continue(); } else res(out); }; c.onerror = () => rej(c.error); }); };
SCR.flushCanton = async (job) => {
  const all = await SCR.idbAll(); const pre = `cne2025_d${job.dignidad}_c${job.canton}_p`;
  const parts = Object.keys(all).filter(k => k.startsWith(pre) && k.endsWith('.csv')).sort();
  if (!parts.length) return 0;
  SCR.download(`cne2025_d${job.dignidad}_c${job.canton}_CANTON.csv`, parts.map((k, i) => i ? all[k].split('\n').slice(1).join('\n') : all[k]).join(''));
  return parts.length;
};

// --- main loop --------------------------------------------------------------------------
SCR.JOBS = [80, 65, 70, 75, 895, 896, 770, 60].map(c => ({ dignidad: 11, provincia: 17, canton: c }));

SCR.runAll = async (jobs) => {
  const B = '/resultados2025/';
  SCR.state = { started: new Date().toISOString(), job: null, parroquia: null, juntasDone: 0, juntasThis: 0, failJ: [], finished: null };
  for (const job of jobs) {
    SCR.state.job = job;
    const parrs = await SCR.post(B + 'ResultadosSeccionales/CatalogoParroquiaJson', { intCanton: job.canton, intCircunscripcion: -1, intDignidad: job.dignidad });
    for (const pa of parrs) {
      const key = `${job.dignidad}|${job.canton}|${pa.intCodigo}`;
      if (SCR.ls.get()[key]) continue;                       // resume: parroquia already done
      SCR.state.parroquia = pa.strNombre;
      const juntas = [];
      let zonas = await SCR.post(B + 'Resultados/CatalogoZonaJson', { intParroquia: pa.intCodigo, intCircunscripcion: 0 });
      if (!zonas.length) zonas = [{ intCodigo: 0, strNombre: '(none)' }];
      for (const z of zonas) {
        const js = await SCR.post(B + 'ResultadosSeccionales/CatalogoJuntaJson', { intZona: z.intCodigo, intParroquia: pa.intCodigo, intCircunscripcion: 0 });
        js.forEach(x => juntas.push({ parroquia_codigo: pa.intCodigo, parroquia: pa.strNombre, zona_codigo: z.intCodigo, zona: z.strNombre, junta_id: x.intCodigo, junta: x.strNombre }));
      }
      const rows = [], fail = [];
      SCR.state.juntasThis = 0;
      for (const ju of juntas) {
        const q = { Dignidad: String(job.dignidad), AbrirActa: true, Provincia: String(job.provincia), Circunscripcion: '-1', Canton: String(job.canton),
          Parroquia: String(ju.parroquia_codigo), Zona: String(ju.zona_codigo), Junta: String(ju.junta_id), nomDignidad: '', strJurisdiccion: '', tokenCaptcha: '', Pregunta: '-1' };
        try {
          const j = await SCR.post(B + 'Resultados/ConsultaInformacionResultados', { objConsulta: q }, true);
          if (j.Error || !(j.datos || []).length) fail.push({ ...job, ...ju, msg: j.MensajeError || 'sin datos' });
          else rows.push(...SCR.parse(j, { dignidad: job.dignidad, provincia: job.provincia, canton: job.canton, ...ju }));
        } catch (e) { if (SCR.blocked) throw e; fail.push({ ...job, ...ju, msg: String(e) }); }
        SCR.state.juntasThis++; SCR.state.juntasDone++;
      }
      const fn = SCR.fname(job.dignidad, job.canton, pa.intCodigo, pa.strNombre);
      await SCR.idbPut(fn, SCR.csv(rows));
      if (fail.length) await SCR.idbPut(fn.replace('.csv', '_FALLAS.json'), JSON.stringify(fail));
      SCR.state.failJ.push(...fail);
      const d = SCR.ls.get(); d[key] = { file: fn, juntas: juntas.length, rows: rows.length, fail: fail.length, at: new Date().toISOString() }; SCR.ls.set(d);
      SCR.log.push({ t: new Date().toISOString(), key, file: fn, juntas: juntas.length, rows: rows.length, fail: fail.length });
      await SCR.sleep(1500);
    }
    const fk = 'scr25_flushed', fl = (() => { try { return JSON.parse(localStorage.getItem(fk) || '{}'); } catch (e) { return {}; } })();
    if (!fl[job.canton]) { await SCR.flushCanton(job); fl[job.canton] = 1; try { localStorage.setItem(fk, JSON.stringify(fl)); } catch (e) {} }  // one file per canton
  }
  SCR.state.finished = new Date().toISOString();
};

// supervisor: resume after an error (finished parroquias are skipped); stop if the firewall blocks
SCR.supervise = async () => {
  for (let i = 0; i < 100 && !SCR.blocked; i++) {
    try { await SCR.runAll(SCR.JOBS); if (SCR.state.finished) return; }
    catch (e) { (SCR.restarts = SCR.restarts || []).push({ t: new Date().toISOString(), err: String(e) }); }
    if (!SCR.blocked) await SCR.sleep(60000);
  }
};

// parroquias listed in the catalog with a code that has no juntas: scrape them under the code that does
SCR.EXTRA = [{ dignidad: 11, provincia: 17, canton: 65, pa: { intCodigo: 4765, strNombre: 'JUAN MONTALVO' } }];
SCR.runExtra = async () => { const B = '/resultados2025/';
  for (const job of SCR.EXTRA) { const pa = job.pa; const key = `${job.dignidad}|${job.canton}|${pa.intCodigo}`; if (SCR.ls.get()[key]) continue;
    const juntas = []; let zonas = await SCR.post(B + 'Resultados/CatalogoZonaJson', { intParroquia: pa.intCodigo, intCircunscripcion: 0 });
    if (!zonas.length) zonas = [{ intCodigo: 0, strNombre: '(none)' }];
    for (const z of zonas) { const js = await SCR.post(B + 'ResultadosSeccionales/CatalogoJuntaJson', { intZona: z.intCodigo, intParroquia: pa.intCodigo, intCircunscripcion: 0 });
      js.forEach(x => juntas.push({ parroquia_codigo: pa.intCodigo, parroquia: pa.strNombre, zona_codigo: z.intCodigo, zona: z.strNombre, junta_id: x.intCodigo, junta: x.strNombre })); }
    const rows = [], fail = [];
    for (const ju of juntas) { const q = { Dignidad: String(job.dignidad), AbrirActa: true, Provincia: String(job.provincia), Circunscripcion: '-1', Canton: String(job.canton),
        Parroquia: String(ju.parroquia_codigo), Zona: String(ju.zona_codigo), Junta: String(ju.junta_id), nomDignidad: '', strJurisdiccion: '', tokenCaptcha: '', Pregunta: '-1' };
      try { const j = await SCR.post(B + 'Resultados/ConsultaInformacionResultados', { objConsulta: q }, true);
        if (j.Error || !(j.datos || []).length) fail.push({ ...ju, msg: j.MensajeError || 'sin datos' });
        else rows.push(...SCR.parse(j, { dignidad: job.dignidad, provincia: job.provincia, canton: job.canton, ...ju })); }
      catch (e) { if (SCR.blocked) throw e; fail.push({ ...ju, msg: String(e) }); } }
    const fn = SCR.fname(job.dignidad, job.canton, pa.intCodigo, pa.strNombre); const text = SCR.csv(rows);
    await SCR.idbPut(fn, text); SCR.download(fn, text);
    const d = SCR.ls.get(); d[key] = { file: fn, juntas: juntas.length, rows: rows.length, fail: fail.length, at: new Date().toISOString(), extra: true }; SCR.ls.set(d); } };

SCR.progress = () => { const v = Object.values(SCR.ls.get());
  return { ...SCR.state, failJ: (SCR.state.failJ || []).length, parroquiasDone: v.length, juntasTotal: v.reduce((a, x) => a + x.juntas, 0), nreq: SCR.nreq, errs: SCR.errors.length, blocked: SCR.blocked || null, restarts: (SCR.restarts || []).length }; };
