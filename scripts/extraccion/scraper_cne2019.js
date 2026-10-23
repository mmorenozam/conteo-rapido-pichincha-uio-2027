/*
 * Extraction of the CNE 2019 seccionales results by junta receptora del voto.
 *
 * Where it runs: in the browser console of a page opened at
 *   https://app01.cne.gob.ec/Resultados2019/
 * The 2019 site is an ASP.NET WebForms page (one form, __VIEWSTATE / __EVENTVALIDATION,
 * an UpdatePanel 'UpdatePanel1' under 'ScriptManager1'). There is no JSON API: every
 * selection is a postback. This script sends the same asynchronous postbacks the page
 * sends when a person picks a value in each list and presses CONSULTAR, and reads the
 * results tables from the returned panel. No captcha is involved.
 *
 * Postback chain (async, header X-MicrosoftAjax: Delta=true):
 *   comboDignidad -> comboPro -> comboCanton -> comboParro -> comboZona -> btnConsultar
 *   with comboJunta set to the junta id. The answer holds:
 *   "Actas Procesadas (n) / Válidas (n) / Inconsistentes (n) / En proceso de validación (n)",
 *   "Sufragantes n / Ausentismo n", "Blancos n / Nulos n" and table 'tablaCandi'
 *   (party, candidate, votes).
 *
 * Codes: dignidad 2 = Prefecto, 4 = Alcalde; provincia 17 = Pichincha;
 *   cantones 60 Quito, 65 Cayambe, 70 Mejía, 75 Pedro Moncayo, 80 Rumiñahui,
 *   770 S. M. de los Bancos, 895 P. V. Maldonado, 896 Puerto Quito.
 *
 * Politeness: one request at a time, 700 ms pause before each (same as 2023), up to 6
 * retries with growing back-off. Requests time out after 30 s (120 s for the slow
 * dignidad / provincia / parroquia changes and for rebuilding the chain); a timed-out
 * request is retried as is, other failures rebuild the selection chain from a fresh page.
 * A supervisor resumes the run after an error. If the firewall answers with its check
 * page, the run stops (it is never bypassed).
 *
 * Outcome (19-20 Oct 2026): the server was slow (median 3 to 6 s per answer, a third of
 * the answers took over 10 s) and from about 01:15 UTC the firewall started rejecting the
 * session. The run was stopped. Extracted: Prefecto, 806 juntas in 5 cantons (Rumiñahui,
 * Cayambe, Mejía, Pedro Moncayo, P. V. Maldonado), all matching the SPSS file. Not
 * extracted: Prefecto in Quito, Puerto Quito, S. M. de los Bancos; Alcalde Quito.
 *
 * Resumed 20 Oct 2026 (about 19:20 UTC) from the checkpoints, same pace, at the owner's request.
 *
 * Checkpoints: each finished parroquia is stored in IndexedDB 'scr_cne2019' and recorded
 * in localStorage ('scr19_done') so a restart skips it. When a canton finishes, one CSV
 * is downloaded: cne2019_d<dig>_c<canton>_CANTON.csv. Consolidate with
 * scripts/extraccion/consolidar_juntas_2019.py.
 *
 * Use: paste this file in the console, then
 *   S19.sup = S19.supervise();   // start or resume
 *   S19.progress();         // status
 */
window.S19 = window.S19 || {};
S19.U = '/Resultados2019/';
S19.delay = 700;
S19.nreq = 0; S19.errors = []; S19.log = [];
S19.sleep = ms => new Promise(r => setTimeout(r, ms));
S19.parse = t => new DOMParser().parseFromString(t, 'text/html');
S19.fields = d => { const f = {}; d.querySelectorAll('input[type=hidden],input[type=text]').forEach(i => { if (i.name) f[i.name] = i.value; });
  d.querySelectorAll('select').forEach(s => { if (s.name) f[s.name] = s.value; });
  const r = d.querySelector('input[name=radioDignidad]:checked'); if (r) f.radioDignidad = r.value; return f; };
S19.opts = (d, id) => { const s = d.getElementById(id); return s ? [...s.options].map(o => [o.value, o.text.trim()]) : []; };

// --- async postbacks ------------------------------------------------------------------
S19.delta = tx => { const out = []; let i = 0;
  while (i < tx.length) { const a = tx.indexOf('|', i); if (a < 0) break; const len = +tx.slice(i, a);
    const b = tx.indexOf('|', a + 1); const c = tx.indexOf('|', b + 1);
    out.push({ type: tx.slice(a + 1, b), id: tx.slice(b + 1, c), content: tx.substr(c + 1, len) }); i = c + 1 + len + 1; }
  return out; };
S19.timeout = 30000;
S19.slowTargets = ['comboParro', 'comboDignidad', 'comboPro'];
S19.once = async (target, set, extra) => {
  if (S19.blocked) throw new Error('BLOCKED');
  await S19.sleep(S19.delay); S19.nreq++;
  const f = { ...S19.f, ...set, __EVENTTARGET: target || '', __EVENTARGUMENT: '', __ASYNCPOST: 'true',
    ScriptManager1: 'UpdatePanel1|' + (target || Object.keys(extra || {})[0]), ...(extra || {}) };
  const ms = S19.slowTargets.includes(target) ? Math.max(S19.timeout, 120000) : S19.timeout;
  const ac = new AbortController(); const to = setTimeout(() => ac.abort(), ms);
  try {
    const r = await fetch(S19.U, { method: 'POST', signal: ac.signal, body: new URLSearchParams(f),
      headers: { 'Content-Type': 'application/x-www-form-urlencoded; charset=utf-8', 'X-MicrosoftAjax': 'Delta=true', 'X-Requested-With': 'XMLHttpRequest' } });
    const tx = await r.text();
    if (/_Incapsula_Resource|Incapsula incident/.test(tx)) { S19.blocked = new Date().toISOString(); throw new Error('BLOCKED'); }
    if (r.status !== 200) throw new Error('HTTP ' + r.status);
    const parts = S19.delta(tx);
    const err = parts.find(p => p.type === 'error'); if (err) throw new Error('server: ' + err.content.slice(0, 200));
    const up = parts.find(p => p.type === 'updatePanel'); if (!up) throw new Error('no panel: ' + tx.slice(0, 120));
    const d = S19.parse('<html><body>' + up.content + '</body></html>');
    const hid = {}; parts.filter(p => p.type === 'hiddenField').forEach(p => hid[p.id] = p.content);
    S19.f = { ...S19.f, ...S19.fields(d), ...hid }; S19.doc = d; return d;
  } finally { clearTimeout(to); }
};
S19.init = async () => { const ac = new AbortController(); const to = setTimeout(() => ac.abort(), 120000);
  try { const t = await fetch(S19.U, { signal: ac.signal }).then(r => r.text());
    if (/_Incapsula_Resource|Incapsula incident/.test(t) && !/comboDignidad/.test(t)) { S19.blocked = new Date().toISOString(); throw new Error('BLOCKED'); }
    S19.doc = S19.parse(t); S19.f = S19.fields(S19.doc); } finally { clearTimeout(to); } };
// rebuild the selection chain (dignidad, provincia, canton, parroquia, zona) from a fresh page
S19.rebuild = async () => { const c = S19.ctx; const keep = S19.timeout; S19.timeout = 120000;
  try { await S19.init();
    await S19.once('comboDignidad', { comboDignidad: c.dig });
    await S19.once('comboPro', { comboPro: c.pro });
    if (c.canton) await S19.once('comboCanton', { comboCanton: c.canton });
    if (c.parro) await S19.once('comboParro', { comboParro: c.parro });
    if (c.zona != null) await S19.once('comboZona', { comboZona: c.zona });
  } finally { S19.timeout = keep; } };
S19.post = async (target, set, extra) => {
  for (let a = 0; a < 6; a++) {
    try { return await S19.once(target, set, extra); }
    catch (e) { S19.errors.push({ target, set, err: String(e), a, t: new Date().toISOString() });
      if (S19.blocked) throw e;
      await S19.sleep(3000 * (a + 1) ** 2);
      // a timed-out request is retried as is; other failures (or repeated timeouts) rebuild the chain
      if (!/Abort/.test(String(e)) || a >= 3) { try { await S19.rebuild(); } catch (e2) { S19.errors.push({ rebuild: String(e2), t: new Date().toISOString() }); } } }
  }
  throw new Error('failed ' + target + ' ' + JSON.stringify(set));
};

// --- parse one junta answer ----------------------------------------------------------
S19.res = d => {
  const tabs = [...d.querySelectorAll('table')].map(t => t.textContent.replace(/\s+/g, ' '));
  const T = s => tabs.find(t => t.includes(s)) || '';
  const n = (t, re) => { const m = t.match(re); return m ? Number(m[1].replace(/\./g, '')) : null; };
  const ts = T('Ausentismo'), tb = T('Nulos'), ta = T('Actas Procesadas');
  const tc = d.getElementById('tablaCandi');
  return { sufragantes: n(ts, /Sufragantes ([\d\.]+)/), ausentismo: n(ts, /Ausentismo ([\d\.]+)/),
    blancos: n(tb, /Blancos ([\d\.]+)/), nulos: n(tb, /Nulos ([\d\.]+)/),
    actas_procesadas: n(ta, /Procesadas \((\d+)\)/), actas_validas: n(ta, /V.lidas \((\d+)\)/),
    actas_inconsistentes: n(ta, /Inconsistentes \((\d+)\)/), actas_en_validacion: n(ta, /validaci.n \((\d+)\)/),
    cand: tc ? [...tc.rows].slice(1).filter(r => r.cells.length >= 4).map(r => ({ op_nombre: r.cells[1].textContent.trim(),
      candidato: r.cells[2].textContent.trim(), votos: Number(r.cells[3].textContent.trim().replace(/\./g, '')) })) : [] };
};

// --- CSV, storage and downloads -------------------------------------------------------
S19.COLS = ['dignidad','provincia','canton','canton_nombre','parroquia_codigo','parroquia','zona_codigo','zona','junta_id','junta','sexo',
  'sufragantes','ausentismo','blancos','nulos','actas_procesadas','actas_validas','actas_inconsistentes','actas_en_validacion',
  'op_orden','op_nombre','candidato','votos'];
S19.csv = rows => { const q = v => v == null ? '' : (typeof v === 'string' && /[",\n;]/.test(v) ? '"' + v.replace(/"/g, '""') + '"' : String(v));
  return [S19.COLS.join(','), ...rows.map(r => S19.COLS.map(c => q(r[c])).join(','))].join('\n') + '\n'; };
S19.download = (name, text) => { const a = document.createElement('a'); a.href = URL.createObjectURL(new Blob([text], { type: 'text/csv;charset=utf-8' }));
  a.download = name; document.body.appendChild(a); a.click(); setTimeout(() => { URL.revokeObjectURL(a.href); a.remove(); }, 5000); };
S19.ls = { get: () => { try { return JSON.parse(localStorage.getItem('scr19_done') || '{}'); } catch (e) { return {}; } },
           set: o => { try { localStorage.setItem('scr19_done', JSON.stringify(o)); } catch (e) {} } };
S19.idb = () => new Promise((res, rej) => { const r = indexedDB.open('scr_cne2019', 1); r.onupgradeneeded = () => r.result.createObjectStore('files'); r.onsuccess = () => res(r.result); r.onerror = () => rej(r.error); });
S19.idbPut = async (k, v) => { const db = await S19.idb(); return new Promise((res, rej) => { const tx = db.transaction('files', 'readwrite'); tx.objectStore('files').put(v, k); tx.oncomplete = () => res(true); tx.onerror = () => rej(tx.error); }); };
S19.idbAll = async () => { const db = await S19.idb(); return new Promise((res, rej) => { const out = {}; const c = db.transaction('files', 'readonly').objectStore('files').openCursor();
  c.onsuccess = e => { const cur = e.target.result; if (cur) { out[cur.key] = cur.value; cur.continue(); } else res(out); }; c.onerror = () => rej(c.error); }); };
S19.flushed = { get: () => { try { return JSON.parse(localStorage.getItem('scr19_flushed') || '{}'); } catch (e) { return {}; } },
                set: o => { try { localStorage.setItem('scr19_flushed', JSON.stringify(o)); } catch (e) {} } };
S19.flushCanton = async (dig, canton) => {
  const fl = S19.flushed.get(); if (fl[dig + '|' + canton]) return 0;   // already downloaded
  fl[dig + '|' + canton] = 1; S19.flushed.set(fl);
  const all = await S19.idbAll(); const pre = `cne2019_d${dig}_c${canton}_p`;
  const parts = Object.keys(all).filter(k => k.startsWith(pre) && k.endsWith('.csv')).sort();
  const fails = Object.keys(all).filter(k => k.startsWith(pre) && k.endsWith('_FALLAS.json')).flatMap(k => JSON.parse(all[k]));
  if (!parts.length) return 0;
  S19.download(`cne2019_d${dig}_c${canton}_CANTON.csv`, parts.map((k, i) => i ? all[k].split('\n').slice(1).join('\n') : all[k]).join(''));
  if (fails.length) S19.download(`cne2019_d${dig}_c${canton}_FALLAS.json`, JSON.stringify(fails, null, 1));
  return parts.length;
};

// --- main loop --------------------------------------------------------------------------
// Rumiñahui first as the pilot canton, Quito last; then Alcalde Quito.
S19.JOBS = [80, 65, 70, 75, 895, 896, 770, 60].map(c => ({ dig: '2', pro: '17', canton: String(c) }))
  .concat([{ dig: '4', pro: '17', canton: '60' }]);

S19.runAll = async (jobs) => {
  S19.state = { started: new Date().toISOString(), job: null, parroquia: null, juntasDone: 0, juntasThis: 0, failJ: [], finished: null };
  for (const job of jobs) {
    if (S19.flushed.get()[job.dig + '|' + job.canton]) continue;   // canton already downloaded: no requests
    S19.state.job = job;
    S19.ctx = { dig: job.dig, pro: job.pro, canton: job.canton };
    await S19.rebuild();
    const cantonNombre = (S19.opts(S19.doc, 'comboCanton').find(o => o[0] === job.canton) || [, ''])[1];
    const parrs = S19.opts(S19.doc, 'comboParro').filter(o => o[0] !== '-1');
    for (const [pc, pn] of parrs) {
      const key = `${job.dig}|${job.canton}|${pc}`;
      if (S19.ls.get()[key]) continue;                         // resume: parroquia already done
      S19.state.parroquia = pn; S19.state.juntasThis = 0;
      S19.ctx = { ...S19.ctx, parro: pc, zona: null };
      await S19.post('comboParro', { comboParro: pc });
      const zonas = S19.opts(S19.doc, 'comboZona').filter(o => o[0] !== '-1');
      const rows = [], fail = [], seen = new Set();
      for (const [zc, zn] of zonas) {
        S19.ctx.zona = zc;
        await S19.post('comboZona', { comboZona: zc });
        const juntas = S19.opts(S19.doc, 'comboJunta').filter(o => o[0] !== '-1' && !seen.has(o[0]));
        for (const [jc, jn] of juntas) {
          seen.add(jc);
          const meta = { dignidad: job.dig, provincia: job.pro, canton: job.canton, canton_nombre: cantonNombre, parroquia_codigo: pc, parroquia: pn,
            zona_codigo: zc, zona: zn, junta_id: jc, junta: jn, sexo: /F$/.test(jn) ? 'F' : (/M$/.test(jn) ? 'M' : '') };
          let ok = false, msg = '';
          for (let a = 0; a < 2 && !ok; a++) {
            try {
              const d = await S19.post('', { comboJunta: jc }, { btnConsultar: 'CONSULTAR' });
              const sel = (d.getElementById('comboJunta') || {}).value;
              const r = S19.res(d);
              if (sel !== jc) msg = 'junta seleccionada ' + sel;
              else if (r.sufragantes == null || !r.cand.length) msg = 'sin datos';
              else { const { cand, ...head } = r; cand.forEach((c, i) => rows.push({ ...meta, ...head, op_orden: i + 1, ...c })); ok = true; }
            } catch (e) { if (S19.blocked) throw e; msg = String(e); }
            if (!ok) { S19.errors.push({ junta: jc, msg, t: new Date().toISOString() }); await S19.rebuild().catch(() => {}); }
          }
          if (!ok) fail.push({ ...meta, msg });
          S19.state.juntasThis++; S19.state.juntasDone++;
        }
      }
      const fn = `cne2019_d${job.dig}_c${job.canton}_p${pc}.csv`;
      await S19.idbPut(fn, S19.csv(rows));
      if (fail.length) await S19.idbPut(fn.replace('.csv', '_FALLAS.json'), JSON.stringify(fail));
      S19.state.failJ.push(...fail);
      const dn = S19.ls.get(); dn[key] = { file: fn, parroquia: pn, juntas: seen.size, rows: rows.length, fail: fail.length, at: new Date().toISOString() }; S19.ls.set(dn);
      S19.log.push({ t: new Date().toISOString(), key, parroquia: pn, juntas: seen.size, rows: rows.length, fail: fail.length });
      await S19.sleep(1500);
    }
    await S19.flushCanton(job.dig, job.canton);                // one file per canton
  }
  S19.state.finished = new Date().toISOString();
};

// supervisor: resume after an error (finished parroquias are skipped); stop if the firewall blocks
S19.supervise = async () => {
  for (let i = 0; i < 200 && !S19.blocked; i++) {
    try { await S19.runAll(S19.JOBS); if (S19.state.finished) return; }
    catch (e) { (S19.restarts = S19.restarts || []).push({ t: new Date().toISOString(), err: String(e) }); }
    if (!S19.blocked) await S19.sleep(60000);
  }
};

S19.progress = () => { const v = Object.values(S19.ls.get());
  return { ...S19.state, failJ: (S19.state.failJ || []).length, parroquiasDone: v.length, juntasTotal: v.reduce((a, x) => a + x.juntas, 0),
    nreq: S19.nreq, errs: S19.errors.length, blocked: S19.blocked || null, restarts: (S19.restarts || []).length, last: S19.log.slice(-3) }; };
