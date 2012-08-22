(*trecs‚ªbottleneck?*)
let q0 = 0
let qr = 1
let qw = 2
let qrw = 3
let qc = 4
let tr_newr k q = if q=q0 then k qr else if q=qw then k qrw else assert false
let tr_neww k q = if q=q0 then k qw else if q=qr then k qrw else assert false
let tr_read k q = if q=qr then k qr else if q=qrw then k qrw else assert false
let tr_write k q = if q=qw then k qw else if q=qrw then k qrw else assert false
let tr_close k q = if q=qr then k qc else if q=qw then k qc else if q=qrw then k qrw else assert false
let tr_end q = if q=qc || q=q0 || q=qrw then () else assert false
let tr_br k1 k2 q = k1 q; k2 q
let br1 = Random.bool ()(*a1=a2*)
let br2 = Random.bool ()(*b1=b2*)
let write y k q = y tr_write k q
let read x k q = x tr_read k q
let close x k q = x tr_close k q
let ii x y q = x y q
let kk x y q = y q
let rec neww k q = if br1 then tr_neww (k ii) q else k kk q
let rec ff x y k q = tr_br (close x (close y k)) (read x (write y (ff x y k))) q
let c2 x y q = ff x y tr_end q
let c1 x q = neww (c2 x) q
let rec newr k q = if br2 then tr_newr (k ii) q else k kk q
let ss q = newr c1 q
let main () = ss 0
