include HorSatInterface

let version () = version_aux !Flag.horsat2 "HorSat2"
let check_apt = check_apt_aux !Flag.horsat2
let check = check_aux !Flag.horsat2
