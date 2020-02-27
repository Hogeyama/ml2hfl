type return =
    {result : CEGAR.result;
     stats : stats option;
     make_get_rtyp : (CEGAR_syntax.var -> CEGAR_ref_type.t) -> Syntax.id -> Ref_type.t;
     set_main: Problem.t option;
     main: Syntax.id option;
     info: Problem.info}

and stats =
  {cycles : int;
   total : float;
   abst : float;
   mc : float;
   refine : float}

let return_of_timeout =
  {result = CEGAR.Unknown "TimeOut";
   stats = None;
   make_get_rtyp = (fun _ -> assert false);
   set_main = None;
   main = None;
   info = Problem.init_info}

type bin_input =
    {args : string list;
     preprocessed : Preprocess.result list}
