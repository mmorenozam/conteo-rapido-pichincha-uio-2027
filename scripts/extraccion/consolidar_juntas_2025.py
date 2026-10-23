"""Consolidate and validate the CNE 2025 (generales, first round) junta-level extraction.

The extraction (scripts/extraccion/scraper_cne2025.js) downloaded one file per canton
(cne2025_d11_c<canton>_CANTON.csv) plus cne2025_d11_c65_p4765_JUAN-MONTALVO.csv (the
site's parroquia catalog lists Juan Montalvo under code 5495, which has no juntas; its
juntas are under 4765, the code used by the SPSS file and the distributivo). This script
merges them, removes duplicates, checks completeness, compares with the official CNE SPSS
file (Primera-Vuelta.sav) at parroquia x sex x candidate level, and attaches the recinto
of each junta from the 2025 distributivo (cut-off 30 Oct 2024).

Usage (from the repo root):
    python scripts/extraccion/consolidar_juntas_2025.py --dignidad 11 \
        --entrada data-raw/pre-consolidacion-2025-presidente \
        --salida  data-raw/pre-consolidacion-2025-presidente \
        --raw     data-raw/generales-2025 \
        --distributivo <30oct2024_DistributivoRecintos-Coord1 converted to .xlsx>

The distributivo is an old .xls; without xlrd, convert it first with
soffice --headless --convert-to xlsx and pass the .xlsx.

Requires pandas, numpy, openpyxl (pyreadstat is not needed: savread.py reads the .sav).
Outputs: cne2025_<nombre>_juntas.csv, cne2025_<nombre>_extraccion_log.json,
         cne2025_<nombre>_validacion.md and, only if there are differences,
         cne2025_<nombre>_diferencias_spss.csv
"""
import argparse, glob, os, json, hashlib, datetime, sys
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from savread import read_sav  # noqa: E402

DIG = {11: ("PRESIDENTE Y VICEPRESIDENTE", "presidente_pichincha", None)}
KEYS = ["parroquia_codigo", "zona_codigo", "sexo", "junta_num"]


def sha(path):
    return hashlib.sha256(open(path, "rb").read()).hexdigest()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dignidad", type=int, required=True, choices=DIG)
    ap.add_argument("--entrada", required=True)
    ap.add_argument("--salida", required=True)
    ap.add_argument("--raw", required=True, help="folder with the CNE 2025 .sav files")
    ap.add_argument("--distributivo", required=True, help="2025 distributivo (.xls or converted .xlsx)")
    a = ap.parse_args()
    dlabel, name, canton_filter = DIG[a.dignidad]
    os.makedirs(a.salida, exist_ok=True)
    log = {"generado": datetime.datetime.now(datetime.timezone.utc).isoformat(),
           "fuente": "https://app01.cne.gob.ec/Resultados2025/ (POST Resultados/ConsultaInformacionResultados, una consulta por junta)",
           "dignidad": dlabel, "archivos_entrada": []}

    # 1. read checkpoint files
    files = sorted(glob.glob(os.path.join(a.entrada, f"cne2025_d{a.dignidad}_c*.csv")))
    parts = []
    for f in files:
        df = pd.read_csv(f, dtype={"op_orden": str, "op_lista": str})
        parts.append(df)
        log["archivos_entrada"].append({"archivo": os.path.basename(f), "filas": len(df),
                                        "juntas": int(df.junta_id.nunique()), "sha256": sha(f)})
    d = pd.concat(parts, ignore_index=True)
    L = {"filas_brutas": len(d)}

    # 2. duplicates (canton files can overlap parroquia files)
    key = ["junta_id", "candidato"]
    L["conflictos_duplicados"] = int((d.groupby(key).votos.nunique() > 1).sum())
    d = d.drop_duplicates(key, keep="first").copy()
    L["filas"] = len(d)
    d["junta_num"] = d.junta.str.extract(r"(\d+)")[0].astype(int)
    d["sexo"] = d.junta.str[-1]
    heads = d.drop_duplicates("junta_id")
    L["juntas"] = int(len(heads))
    L["parroquias"] = int(heads.parroquia_codigo.nunique())

    # 3. official SPSS file
    r, m = read_sav(os.path.join(a.raw, "Primera-Vuelta.sav"))
    sp = r[(r.DIGNIDAD_CODIGO == a.dignidad) & (r.PROVINCIA_CODIGO == 17)].copy()
    sp["CANDIDATO_NOMBRE"] = sp.CANDIDATO_NOMBRE.str.strip()
    if canton_filter:
        sp = sp[sp.CANTON_CODIGO == canton_filter]
    exp_parr, got_parr = set(sp.PARROQUIA_CODIGO.astype(int)), set(heads.parroquia_codigo.astype(int))
    L["parroquias_faltantes"] = sorted(exp_parr - got_parr)
    L["parroquias_extra"] = sorted(got_parr - exp_parr)

    # 4. internal consistency of each acta
    gap = d.groupby("junta_id").votos.sum() + heads.set_index("junta_id").eval("blancos + nulos - sufragantes")
    L["juntas_suma_inconsistente"] = int((gap != 0).sum())
    L["brecha_neta_votos"] = int(gap.sum())
    L["actas"] = {k: int(heads[f"actas_{k}"].fillna(0).sum()) for k in ["validas", "novedad", "por_procesar"]}

    # 5. comparison with SPSS (parroquia x sexo x candidato) and header totals
    sp = sp.assign(sexo=sp.JUNTA_SEXO.str[0], parroquia=sp.PARROQUIA_CODIGO.astype(int))
    s1 = sp.groupby(["parroquia", "sexo", "CANDIDATO_NOMBRE"]).VOTOS.sum()
    s2 = d.assign(parroquia=d.parroquia_codigo.astype(int)).groupby(["parroquia", "sexo", "candidato"]).votos.sum()
    s1.index.names = s2.index.names = ["parroquia", "sexo", "candidato"]
    cmp = pd.concat([s1.rename("spss"), s2.rename("web")], axis=1)
    diff = cmp[cmp.spss != cmp.web]
    L["celdas_comparadas"], L["celdas_distintas"] = int(len(cmp)), int(len(diff))
    L["votos_validos_spss"], L["votos_validos_web"] = int(cmp.spss.sum()), int(cmp.web.sum())
    hs = sp.drop_duplicates(["parroquia", "sexo"])[["SUFRAGANTES", "BLANCOS", "NULOS"]].sum()
    L["spss_sufragantes_blancos_nulos"] = [int(x) for x in hs]
    L["web_sufragantes_blancos_nulos"] = [int(heads[c].sum()) for c in ["sufragantes", "blancos", "nulos"]]
    if len(diff):
        diff.reset_index().to_csv(os.path.join(a.salida, f"cne2025_{name}_diferencias_spss.csv"), index=False)

    # 6. recinto of each junta from the 2025 distributivo (junta ranges by sex)
    dist = pd.read_excel(a.distributivo, sheet_name="DISTRIBUTIVO al 30102024", header=5)
    dist.columns = [str(c).strip().replace(" ", "_") for c in dist.columns]
    dist = dist[dist.CODIGO_PROVINCIA == 17].assign(U_R=None)  # the 2025 distributivo has no U/R column
    rows = []
    for _, x in dist.iterrows():
        for sx, i0, i1 in [("F", x.JUN_INIF, x.JUN_FINF), ("M", x.JUN_INIM, x.JUN_FINM)]:
            if pd.notna(i0) and i0 > 0:
                rows += [(x.CODIGO_PARROQUIA, x.CODIGO_ZONA, sx, k, x.CODIGO_RECINTO, x.NOMBRE_RECINTO, x.U_R)
                         for k in range(int(i0), int(i1) + 1)]
    rng = pd.DataFrame(rows, columns=KEYS + ["recinto_codigo", "recinto", "recinto_u_r"])
    L["distributivo_rangos_duplicados"] = int(rng.duplicated(KEYS, keep=False).sum())
    rng = rng.drop_duplicates(KEYS, keep=False)
    d = d.merge(rng, on=KEYS, how="left", validate="m:1")
    hj = d.drop_duplicates("junta_id")
    L["juntas_con_recinto"] = int(hj.recinto_codigo.notna().sum())
    L["recintos"] = int(hj.recinto_codigo.nunique())
    L["juntas_sin_recinto"] = hj[hj.recinto_codigo.isna()][
        ["parroquia_codigo", "parroquia", "zona_codigo", "zona", "junta", "sufragantes"]].to_dict("records")
    exp_j = rng[rng.parroquia_codigo.isin(got_parr)]
    have = set(map(tuple, hj[KEYS].to_numpy()))
    miss = exp_j[[tuple(k) not in have for k in exp_j[KEYS].to_numpy()]]
    L["juntas_distributivo"] = int(len(exp_j))
    L["juntas_distributivo_sin_resultado"] = miss[KEYS + ["recinto"]].to_dict("records")

    # 7. write
    cols = ["dignidad", "provincia", "canton", "parroquia_codigo", "parroquia", "zona_codigo", "zona", "recinto_codigo",
            "recinto", "recinto_u_r", "junta_id", "junta", "junta_num", "sexo", "sufragantes", "ausentismo", "blancos",
            "nulos", "actas_validas", "actas_novedad", "actas_por_procesar", "op_codigo", "op_lista", "op_orden",
            "op_nombre", "candidato", "votos"]
    d = d.sort_values(["canton", "parroquia_codigo", "zona_codigo", "sexo", "junta_num", "op_orden"])
    out = os.path.join(a.salida, f"cne2025_{name}_juntas.csv")
    d[cols].to_csv(out, index=False)
    L["archivo_consolidado"], L["sha256"] = os.path.basename(out), sha(out)
    log["resultado"] = L
    json.dump(log, open(os.path.join(a.salida, f"cne2025_{name}_extraccion_log.json"), "w", encoding="utf-8"),
              ensure_ascii=False, indent=1, default=str)

    md = [f"# Validación de la extracción CNE 2025 por junta: {dlabel}", "",
          f"Generado: {log['generado']}", "",
          f"- Archivos de entrada: {len(files)}; filas brutas {L['filas_brutas']:,}; filas finales {L['filas']:,}; duplicados con conflicto: {L['conflictos_duplicados']}.",
          f"- Juntas: {L['juntas']:,} en {L['parroquias']} parroquias. Parroquias faltantes frente al SPSS: {L['parroquias_faltantes'] or 'ninguna'}; extra: {L['parroquias_extra'] or 'ninguna'}.",
          f"- Comparación con el SPSS del CNE (parroquia × sexo × candidato): {L['celdas_comparadas']:,} celdas, {L['celdas_distintas']} distintas. Votos válidos: SPSS {L['votos_validos_spss']:,}, web {L['votos_validos_web']:,}.",
          f"- Sufragantes, blancos, nulos: SPSS {L['spss_sufragantes_blancos_nulos']}, web {L['web_sufragantes_blancos_nulos']}.",
          f"- Actas: {L['actas']}. Juntas donde válidos + blancos + nulos ≠ sufragantes: {L['juntas_suma_inconsistente']} (brecha neta {L['brecha_neta_votos']} votos).",
          f"- Recintos: {L['juntas_con_recinto']:,} juntas unidas a {L['recintos']} recintos del distributivo 2025. Juntas sin recinto: {len(L['juntas_sin_recinto'])}. Juntas del distributivo sin resultado: {len(L['juntas_distributivo_sin_resultado'])}.",
          ""]
    if L["juntas_sin_recinto"]:
        md += ["Juntas sin recinto (juntas especiales que no constan en el distributivo):", "",
               "| Parroquia | Zona | Junta | Sufragantes |", "| --- | --- | --- | --- |"]
        md += [f"| {x['parroquia']} | {x['zona']} | {x['junta']} | {x['sufragantes']} |" for x in L["juntas_sin_recinto"]]
    open(os.path.join(a.salida, f"cne2025_{name}_validacion.md"), "w", encoding="utf-8").write("\n".join(md) + "\n")
    print("\n".join(md))


if __name__ == "__main__":
    main()
