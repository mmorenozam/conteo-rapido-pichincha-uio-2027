"""Minimal SPSS .sav reader (bytecode compression), returns pandas DataFrame + metadata."""
import struct
import numpy as np
import pandas as pd


def read_sav(path, encoding=None):
    f = open(path, "rb"); data = f.read(); f.close()
    pos = 0
    hdr = data[:176]; pos = 176
    e = "<" if struct.unpack("<i", hdr[64:68])[0] in (2, 3) else ">"
    compr = struct.unpack(e + "i", hdr[72:76])[0]
    ncases = struct.unpack(e + "i", hdr[80:84])[0]
    bias = struct.unpack(e + "d", hdr[84:92])[0]
    segs = []  # each: dict(type, name, label)
    vlabels = []
    longnames = {}
    vls = {}
    enc = encoding

    def i32():
        nonlocal pos
        v = struct.unpack(e + "i", data[pos:pos + 4])[0]; pos += 4; return v

    while True:
        rt = i32()
        if rt == 2:
            typ = i32(); has_lab = i32(); nmis = i32(); i32(); i32()
            name = data[pos:pos + 8]; pos += 8
            lab = None
            if has_lab:
                ln = i32(); lab = data[pos:pos + ln]; pos += (ln + 3) // 4 * 4
            pos += abs(nmis) * 8
            segs.append(dict(type=typ, name=name, label=lab))
        elif rt == 3:
            n = i32(); labs = []
            for _ in range(n):
                val = data[pos:pos + 8]; pos += 8
                ln = data[pos]; lab = data[pos + 1:pos + 1 + ln]
                pos += (1 + ln + 7) // 8 * 8
                labs.append((val, lab))
            rt4 = i32(); assert rt4 == 4
            nv = i32(); idx = [i32() for _ in range(nv)]
            vlabels.append((idx, labs))
        elif rt == 6:
            n = i32(); pos += 80 * n
        elif rt == 7:
            sub = i32(); size = i32(); cnt = i32()
            blob = data[pos:pos + size * cnt]; pos += size * cnt
            if sub == 13:
                longnames = dict(kv.split(b"=", 1) for kv in blob.split(b"\t") if b"=" in kv)
            elif sub == 14:
                for kv in blob.split(b"\x00\t"):
                    kv = kv.strip(b"\x00\t")
                    if b"=" in kv:
                        k, v = kv.split(b"=", 1); vls[k] = int(v)
            elif sub == 20:
                enc = enc or blob.decode("ascii").strip()
        elif rt == 999:
            i32(); break
        else:
            raise ValueError(f"record {rt} at {pos}")
    enc = enc or "latin1"
    if enc.lower() in ("windows-1252", "cp1252"):
        enc = "cp1252"

    # variables: segments with type != -1
    variables = []
    for k, s in enumerate(segs):
        if s["type"] == -1:
            continue
        nseg = 1 if s["type"] == 0 else (s["type"] + 7) // 8
        short = s["name"].rstrip()
        variables.append(dict(short=short, start=k, nseg=nseg, type=s["type"],
                              label=s["label"].decode(enc, "replace") if s["label"] else None))
    nseg_total = len(segs)

    # decompress data into an array of 8-byte segments
    assert compr == 1, "only bytecode compression supported"
    out = bytearray()
    sysmis = struct.pack(e + "d", -np.finfo(np.float64).max)
    spaces = b" " * 8
    dpos = pos; total_needed = ncases * nseg_total * 8 if ncases > 0 else None
    while dpos < len(data):
        cmds = data[dpos:dpos + 8]; dpos += 8
        done = False
        for c in cmds:
            if c == 0:
                continue
            elif c == 252:
                done = True; break
            elif c == 253:
                out += data[dpos:dpos + 8]; dpos += 8
            elif c == 254:
                out += spaces
            elif c == 255:
                out += sysmis
            else:
                out += struct.pack(e + "d", c - bias)
        if done or (total_needed and len(out) >= total_needed):
            break
    n = len(out) // (8 * nseg_total)
    raw = np.frombuffer(bytes(out[: n * 8 * nseg_total]), dtype="V8").reshape(n, nseg_total)

    cols = {}
    for v in variables:
        name = longnames.get(v["short"], v["short"]).decode(enc, "replace")
        v["name"] = name
        if v["type"] == 0:
            col = raw[:, v["start"]].copy().view(e + "f8").ravel().astype(float)
            col[col <= -np.finfo(np.float64).max] = np.nan
            cols[name] = col
        else:
            b = raw[:, v["start"]:v["start"] + v["nseg"]].tobytes()
            w = v["nseg"] * 8
            cols[name] = [b[i * w:(i + 1) * w][: v["type"]].decode(enc, "replace").rstrip() for i in range(n)]
    df = pd.DataFrame(cols)

    # value labels mapped to variable names (index = 1-based segment index)
    seg2var = {v["start"] + 1: v for v in variables}
    labels = {}
    for idx, labs in vlabels:
        for i in idx:
            v = seg2var.get(i)
            if v is None:
                continue
            m = {}
            for val, lab in labs:
                key = struct.unpack(e + "d", val)[0] if v["type"] == 0 else val.decode(enc, "replace").rstrip()
                m[key] = lab.decode(enc, "replace")
            labels[v["name"]] = m
    meta = dict(encoding=enc, ncases=ncases, var_labels={v["name"]: v["label"] for v in variables}, value_labels=labels)
    return df, meta
