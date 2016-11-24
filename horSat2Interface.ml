include HorSatInterface

let version () = version_aux !Flag.horsat2 "HorSat2"
let check_apt = check_apt_aux !Flag.horsat2 HorSat2_parser.output_apt HorSat2_lexer.token
let check = check_aux !Flag.horsat2 HorSat2_parser.output HorSat2_lexer.token
