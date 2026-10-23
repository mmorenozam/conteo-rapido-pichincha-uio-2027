"""Consolidate and validate the CNE 2019 junta-level extraction for one dignidad.

The extraction (scripts/extraccion/scraper_cne2019.js) downloads one file per canton
(cne2019_d<dig>_c<canton>_CANTON.csv). This script merges them, removes duplicates,
checks completeness, compares with the official CNE SPSS file at parroquia x sex x
candidate level, and attaches the recinto of each junta from the 2019 distributivo.

The 2019 distributivo has names only (no codes for parroquia, zona or recinto), so the
join is by normalized names: canton, parroquia, zona (empty in the distributivo when the
parroquia has no zones, "SIN ZONA" on the results site), sex and junta number inside the
recinto's junta range. Parroquia and zona names that do not match exactly are matched to
the closest name in the same canton / parroquia; every such match is listed in the log.
Recintos get a surrogate id, "R19_<row of the distributivo>", because 2019 has no codes.

Usage (from the repo root):
    python scripts/extraccion/consolidar_juntas_2019.py --dignidad 2 \
        --entrada data-raw/pre-consolidacion-2019-prefecto \
        --salida  data-raw/pre-consolidacion-2019-prefecto \
        --raw     data-raw/seccionales-2019 \
        --distributivo <distrivutivo_recinto converted to .xlsx>

The distributivo is an old .xls; pandas needs xlrd for it. If xlrd is not available,
convert it first (LibreOffice: soffice --headless --convert-to xlsx distrivutivo_recinto.xls)
and pass the .xlsx with --distributivo.

Outputs: cne2019_<nombre>_juntas.csv, cne2019_<nombre>_extraccion_log.json,
         cne2019_<nombre>_validacion.md and, only if there are differences,
         cne2019_<nombre>_diferencias_spss.csv
"""
import argparse, glob, os, json, hashlib, datetime, sys, unicodedata, re, difflib
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from savread import read_sav  # noqa: E402

DIG = {2: ("PREFECTO Y VICEPREFECTO", "prefecto_pichincha", None),
       4: ("ALCALDE", "alcalde_quito", 60)}


def sha(path):
    return hashlib.sha256(open(path, "rb").read()).hexdigest()


def norm(s):
    if s is None or (isinstance(s, float) and pd.isna(s)):
        return ""
    s = unicodedata.normalize("NFD", str(s).upper())
    s = "".join(c for c in s if unicodedata.category(c) != "Mn")
    s = re.sub(r"[^A-Z0-9]+", " ", s).strip()
    return "" if s == "SIN ZONA" else s


def closest(name, pool):
    if name in pool:
        return name, 1.0
    m = difflib.get_close_matches(name, list(pool), n=1, cutoff=0.0)
    return (m[0], difflib.SequenceMatcher(None, name, m[0]).ratio()) if m else (None, 0.0)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dignidad", type=int, required=True, choices=DIG)
    ap.add_argument("--entrada", required=True)
    ap.add_argument("--salida", required=True)
    ap.add_argument("--raw", required=True, help="folder with the CNE 2019 .sav files")
    ap.add_argument("--distributivo", required=True, help="2019 distributivo (.xls or converted .xlsx)")
    a = ap.parse_args()
    dlabel, name, canton_filter = DIG[a.dignidad]
    os.makedirs(a.salida, exist_ok=True)
    log = {"generado": datetime.datetime.now(datetime.timezone.utc).isoformat(),
           "fuente": "https://app01.cne.gob.ec/Resultados2019/ (postback asíncrono btnConsultar, una consulta por junta)",
           "dignidad": dlabel, "archivos_entrada": []}

    # 1. read checkpoint files
    files = sorted(glob.glob(os.path.join(a.entrada, f"cne2019_d{a.dignidad}_c*_CANTON.csv")))
    parts = []
    for f in files:
        df = pd.read_csv(f, dtype={"junta": str, "zona": str})
        parts.append(df)
        log["archivos_entrada"].append({"archivo": os.path.basename(f), "filas": len(df),
                                        "juntas": int(df.junta_id.nunique()), "sha256": sha(f)})
    d = pd.concat(parts, ignore_index=True)
    L = {"filas_brutas": len(d)}

    # 2. duplicates
    key = ["junta_id", "candidato"]
    L["conflictos_duplicados"] = int((d.groupby(key).votos.nunique() > 1).sum())
    d = d.drop_duplicates(key, keep="first").copy()
    L["filas"] = len(d)
    d["junta_num"] = d.junta.str.extract(r"(\d+)")[0].astype(int)
    d["sexo"] = d.junta.str[-1]
    heads = d.drop_duplicates("junta_id")
    L["juntas"] = int(len(heads))
    L["parroquias"] = int(heads.parroquia_codigo.nunique())

    # 3. official SPSS file and candidate catalog
    r, m = read_sav(os.path.join(a.raw, "Resultados-a-nivel-parroquial-1.sav"))
    sp = r[(r.DIGNIDAD_CODIGO == a.dignidad) & (r.PROVINCIA_CODIGO == 17)]
    if canton_filter:
        sp = sp[sp.CANTON_CODIGO == canton_filter]
    # the 2019 extraction is partial (stopped by the site's firewall): compare only the cantons extracted
    L["cantones_extraidos"] = sorted(int(x) for x in heads.canton.unique())
    L["cantones_no_extraidos"] = sorted(int(x) for x in set(sp.CANTON_CODIGO.astype(int)) - set(L["cantones_extraidos"]))
    sp = sp[sp.CANTON_CODIGO.astype(int).isin(L["cantones_extraidos"])]
    c, _ = read_sav(os.path.join(a.raw, "CANDIDATOS.sav"))
    cn = c.drop_duplicates("CANDIDATO_CODIGO").set_index("CANDIDATO_CODIGO").CANDIDATO_NOMBRE
    sp = sp.assign(candidato=sp.CANDIDATO_CODIGO.map(cn).map(lambda s: str(s).strip()))
    L["spss_candidatos_sin_nombre"] = int(sp.candidato.isin(["nan", "None"]).sum())
    exp_parr, got_parr = set(sp.PARROQUIA_CODIGO.astype(int)), set(heads.parroquia_codigo.astype(int))
    L["parroquias_faltantes"] = sorted(exp_parr - got_parr)
    L["parroquias_extra"] = sorted(got_parr - exp_parr)

    # 4. internal consistency of each acta
    gap = d.groupby("junta_id").votos.sum() + heads.set_index("junta_id").eval("blancos + nulos - sufragantes")
    L["juntas_suma_inconsistente"] = int((gap != 0).sum())
    L["brecha_neta_votos"] = int(gap.sum())
    L["actas"] = {k: int(heads[f"actas_{k}"].fillna(0).sum()) for k in ["procesadas", "validas", "inconsistentes", "en_validacion"]}

    # 5. comparison with SPSS (parroquia x sexo x candidato) and header totals
    sp = sp.assign(sexo=sp.JUNTA_SEXO.str[0], parroquia=sp.PARROQUIA_CODIGO.astype(int))
    s1 = sp.groupby(["parroquia", "sexo", "candidato"]).VOTOS.sum()
    s2 = d.assign(parroquia=d.parroquia_codigo.astype(int)).groupby(["parroquia", "sexo", "candidato"]).votos.sum()
    cmp = pd.concat([s1.rename("spss"), s2.rename("web")], axis=1)
    diff = cmp[cmp.spss != cmp.web]
    L["celdas_comparadas"], L["celdas_distintas"] = int(len(cmp)), int(len(diff))
    L["votos_validos_spss"], L["votos_validos_web"] = int(cmp.spss.fillna(0).sum()), int(cmp.web.fillna(0).sum())
    hs = sp.drop_duplicates(["parroquia", "sexo"])[["SUFRAGANTES", "BLANCOS", "NULOS"]].sum()
    L["spss_sufragantes_blancos_nulos"] = [int(x) for x in hs]
    L["web_sufragantes_blancos_nulos"] = [int(heads[c].sum()) for c in ["sufragantes", "blancos", "nulos"]]
    if len(diff):
        diff.reset_index().to_csv(os.path.join(a.salida, f"cne2019_{name}_diferencias_spss.csv"), index=False)

    # 6. recinto of each junta from the 2019 distributivo (names only)
    dist = pd.read_excel(a.distributivo, header=7)
    dist = dist[dist["COD PROVINCIA"] == 17].copy()
    dist["recinto_codigo"] = ["R19_%04d" % (i + 9) for i in dist.index]  # row in the sheet (header on row 8)
    dist["kc"], dist["kp"], dist["kz"] = dist.CANTON.map(norm), dist.PARROQUIA.map(norm), dist["ZONA ELECTORAL"].map(norm)
    rows = []
    for _, x in dist.iterrows():
        for sx, i0, i1 in [("F", x["JUNTA INICIO FEMENINA"], x["JUNTA FIN FEMENINO"]),
                           ("M", x["JUNTA INICIO MASCULINO"], x["JUNTA FIN MASCULINO"])]:
            if pd.notna(i0) and i0 > 0:
                rows += [(x.kc, x.kp, x.kz, sx, k, x.recinto_codigo, x["NOMBRE RECINTO"]) for k in range(int(i0), int(i1) + 1)]
    K = ["kc", "kp", "kz", "sexo", "junta_num"]
    rng = pd.DataFrame(rows, columns=K + ["recinto_codigo", "recinto"])
    L["distributivo_rangos_duplicados"] = int(rng.duplicated(K, keep=False).sum())
    rng = rng.drop_duplicates(K, keep=False)

    # name matching: parroquia within canton, zona within parroquia
    d["kc"], d["kp0"], d["kz0"] = d.canton_nombre.map(norm), d.parroquia.map(norm), d.zona.map(norm)
    pmap, zmap, L["parroquias_emparejadas_aprox"], L["zonas_emparejadas_aprox"] = {}, {}, [], []
    for (kc, kp), _ in d.groupby(["kc", "kp0"]):
        best, sc = closest(kp, set(dist.loc[dist.kc == kc, "kp"]))
        pmap[(kc, kp)] = best
        if best != kp:
            L["parroquias_emparejadas_aprox"].append({"canton": kc, "web": kp, "distributivo": best, "similitud": round(sc, 2)})
    d["kp"] = [pmap[(c_, p_)] for c_, p_ in zip(d.kc, d.kp0)]
    for (kc, kp, kz), _ in d.groupby(["kc", "kp", "kz0"]):
        pool = set(dist.loc[(dist.kc == kc) & (dist.kp == kp), "kz"])
        best, sc = closest(kz, pool)
        if sc < 0.6:
            best = None  # no credible zona: matched below on parroquia, sex and junta number only
        zmap[(kc, kp, kz)] = best
        if best != kz:
            L["zonas_emparejadas_aprox"].append({"canton": kc, "parroquia": kp, "web": kz, "distributivo": best, "similitud": round(sc, 2)})
    d["kz"] = [zmap[(c_, p_, z_)] for c_, p_, z_ in zip(d.kc, d.kp, d.kz0)]
    d = d.merge(rng, on=K, how="left", validate="m:1")
    # fallback for juntas still without recinto: ignore the zona when parroquia + sex + number is unique
    K2 = ["kc", "kp", "sexo", "junta_num"]
    r2 = rng.drop_duplicates(K2, keep=False)[K2 + ["recinto_codigo", "recinto"]]
    nr = d.recinto_codigo.isna()
    fb = d.loc[nr, K2].merge(r2, on=K2, how="left")
    d.loc[nr, "recinto_codigo"], d.loc[nr, "recinto"] = fb.recinto_codigo.to_numpy(), fb.recinto.to_numpy()
    L["juntas_unidas_sin_zona"] = int(d.loc[nr].drop_duplicates("junta_id").recinto_codigo.notna().sum())
    hj = d.drop_duplicates("junta_id")
    L["juntas_con_recinto"] = int(hj.recinto_codigo.notna().sum())
    L["recintos"] = int(hj.recinto_codigo.nunique())
    L["juntas_sin_recinto"] = hj[hj.recinto_codigo.isna()][
        ["canton_nombre", "parroquia", "zona", "junta", "sufragantes"]].to_dict("records")
    exp_j = rng[rng.kc.isin(set(d.kc)) & rng.kp.isin(set(d.kp))]
    have = set(map(tuple, hj[K].to_numpy()))
    miss = exp_j[[tuple(k) not in have for k in exp_j[K].to_numpy()]]
    L["juntas_distributivo"] = int(len(exp_j))
    L["juntas_distributivo_sin_resultado"] = miss[K + ["recinto"]].to_dict("records")

    # 7. write
    cols = ["dignidad", "provincia", "canton", "canton_nombre", "parroquia_codigo", "parroquia", "zona_codigo", "zona",
            "recinto_codigo", "recinto", "junta_id", "junta", "junta_num", "sexo", "sufragantes", "ausentismo", "blancos",
            "nulos", "actas_procesadas", "actas_validas", "actas_inconsistentes", "actas_en_validacion",
            "op_orden", "op_nombre", "candidato", "votos"]
    d = d.sort_values(["canton", "parroquia_codigo", "zona_codigo", "sexo", "junta_num", "op_orden"])
    out = os.path.join(a.salida, f"cne2019_{name}_juntas.csv")
    d[cols].to_csv(out, index=False)
    L["archivo_consolidado"], L["sha256"] = os.path.basename(out), sha(out)
    log["resultado"] = L
    json.dump(log, open(os.path.join(a.salida, f"cne2019_{name}_extraccion_log.json"), "w", encoding="utf-8"),
              ensure_ascii=False, indent=1, default=str)

    md = [f"# Validación de la extracción CNE 2019 por junta: {dlabel}", "",
          f"Generado: {log['generado']}", "",
          f"- Archivos de entrada: {len(files)}; filas brutas {L['filas_brutas']:,}; filas finales {L['filas']:,}; duplicados con conflicto: {L['conflictos_duplicados']}.",
          f"- Juntas: {L['juntas']:,} en {L['parroquias']} parroquias. Parroquias faltantes frente al SPSS: {L['parroquias_faltantes'] or 'ninguna'}; extra: {L['parroquias_extra'] or 'ninguna'}.",
          f"- Comparación con el SPSS del CNE (parroquia × sexo × candidato): {L['celdas_comparadas']:,} celdas, {L['celdas_distintas']} distintas. Votos válidos: SPSS {L['votos_validos_spss']:,}, web {L['votos_validos_web']:,}.",
          f"- Sufragantes, blancos, nulos: SPSS {L['spss_sufragantes_blancos_nulos']}, web {L['web_sufragantes_blancos_nulos']}.",
          f"- Actas: {L['actas']}. Juntas donde válidos + blancos + nulos ≠ sufragantes: {L['juntas_suma_inconsistente']} (brecha neta {L['brecha_neta_votos']} votos).",
          f"- Recintos: {L['juntas_con_recinto']:,} juntas unidas a {L['recintos']} recintos del distributivo 2019 (unión por nombres). Juntas sin recinto: {len(L['juntas_sin_recinto'])}. Juntas del distributivo sin resultado: {len(L['juntas_distributivo_sin_resultado'])}.",
          f"- Nombres emparejados de forma aproximada: {len(L['parroquias_emparejadas_aprox'])} parroquias, {len(L['zonas_emparejadas_aprox'])} zonas (detalle en el log).",
          ""]
    if L["juntas_sin_recinto"]:
        md += ["Juntas sin recinto:", "", "| Cantón | Parroquia | Zona | Junta | Sufragantes |", "| --- | --- | --- | --- | --- |"]
        md += [f"| {x['canton_nombre']} | {x['parroquia']} | {x['zona']} | {x['junta']} | {x['sufragantes']} |" for x in L["juntas_sin_recinto"]]
    open(os.path.join(a.salida, f"cne2019_{name}_validacion.md"), "w", encoding="utf-8").write("\n".join(md) + "\n")
    print("\n".join(md))


if __name__ == "__main__":
    main()
