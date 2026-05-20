(** Mamba "shape" of kubectl 1.31's command surface (subset).

    Goal: validate that mamba can express the parser surface of a real-world
    Cobra-based CLI, and surface any features kubectl uses that mamba lacks.

    What's declared here:
      - Global persistent flags (most-used subset of `kubectl options`)
      - Top-level commands (all 40, but only `get`, `delete`, `config`,
        `version` have full flag detail; the rest get name + description
        with a stubbed run, sufficient for dispatch testing)
      - kubectl config's 14 sub-subcommands

    Reference outputs from the real binary are saved at
    [tests/kubectl_ref/help_*.txt] (kubectl v1.31.0).

    Documented divergences (features kubectl uses, mamba doesn't have):
      - Enumerated string flags: kubectl's [--cascade] only accepts
        {background, orphan, foreground}. mamba could express this with
        [Flag.enum] but kubectl doesn't enforce at parse time, so we keep
        [Flag.string] for fidelity.
      - Long-only short letter on a global flag: [-v] is global verbosity
        in kubectl. mamba accepts this fine, but in mamba [-v] is NOT
        auto-bound to "version" (only [--version] is). So declaring [-v]
        as verbosity doesn't conflict.
      - "kubectl options" pseudo-command: kubectl emits its global flag
        catalog as a synthetic subcommand. mamba's auto-injected help/
        completion commands cover the analogous need; "options" is not
        modeled here. *)

open Mamba
open Test_util

(* ============================================================ *)
(* Stubs                                                         *)
(* ============================================================ *)

let stub _ = Error.success

(* ============================================================ *)
(* Global persistent flags                                       *)
(* ============================================================ *)

let g_namespace =
  Flag.string ~name:"namespace" ~short:'n' ~default:""
    ~doc:"If present, the namespace scope for this CLI request" ()

let g_kubeconfig =
  Flag.string ~name:"kubeconfig" ~default:""
    ~doc:"Path to the kubeconfig file to use for CLI requests" ()

let g_context =
  Flag.string ~name:"context" ~default:""
    ~doc:"The name of the kubeconfig context to use" ()

let g_cluster =
  Flag.string ~name:"cluster" ~default:""
    ~doc:"The name of the kubeconfig cluster to use" ()

let g_verbosity =
  Flag.int ~name:"v" ~short:'v' ~default:0
    ~doc:"number for the log level verbosity" ()

let global_flags =
  [ Flag.pack g_namespace
  ; Flag.pack g_kubeconfig
  ; Flag.pack g_context
  ; Flag.pack g_cluster
  ; Flag.pack g_verbosity
  ]

(* ============================================================ *)
(* kubectl get                                                   *)
(* ============================================================ *)

module Get = struct
  let all_namespaces =
    Flag.bool ~name:"all-namespaces" ~short:'A' ~default:false
      ~doc:"list across all namespaces" ()
  let allow_missing_template_keys =
    Flag.bool ~name:"allow-missing-template-keys" ~default:true
      ~doc:"ignore missing template keys (go-template/jsonpath only)" ()
  let chunk_size =
    Flag.int ~name:"chunk-size" ~default:500
      ~doc:"return large lists in chunks; 0 to disable" ()
  let filename =
    Flag.repeated
      (Flag.string ~name:"filename" ~short:'f'
         ~doc:"file/dir/URL identifying the resource to get" ())
  let field_selector =
    Flag.string ~name:"field-selector" ~default:""
      ~doc:"selector (field query) to filter on" ()
  let ignore_not_found =
    Flag.bool ~name:"ignore-not-found" ~default:false
      ~doc:"return exit code 0 if the requested object does not exist" ()
  let kustomize =
    Flag.string ~name:"kustomize" ~short:'k' ~default:""
      ~doc:"process the kustomization directory" ()
  let label_columns =
    Flag.repeated
      (Flag.string ~name:"label-columns" ~short:'L'
         ~doc:"labels to present as columns" ())
  let no_headers =
    Flag.bool ~name:"no-headers" ~default:false
      ~doc:"omit headers in the default/custom-columns output" ()
  let output =
    Flag.string ~name:"output" ~short:'o' ~default:""
      ~doc:"output format (json|yaml|name|go-template|...)" ()
  let output_watch_events =
    Flag.bool ~name:"output-watch-events" ~default:false
      ~doc:"output watch event objects when --watch is used" ()
  let raw =
    Flag.string ~name:"raw" ~default:""
      ~doc:"raw URI to request from the server" ()
  let recursive =
    Flag.bool ~name:"recursive" ~short:'R' ~default:false
      ~doc:"process -f/--filename directory recursively" ()
  let selector =
    Flag.string ~name:"selector" ~short:'l' ~default:""
      ~doc:"selector (label query) to filter on" ()
  let server_print =
    Flag.bool ~name:"server-print" ~default:true
      ~doc:"have the server return the appropriate table output" ()
  let show_kind =
    Flag.bool ~name:"show-kind" ~default:false
      ~doc:"list the resource type for the requested object(s)" ()
  let show_labels =
    Flag.bool ~name:"show-labels" ~default:false
      ~doc:"show all labels as the last column" ()
  let show_managed_fields =
    Flag.bool ~name:"show-managed-fields" ~default:false
      ~doc:"keep managedFields when printing in JSON or YAML" ()
  let sort_by =
    Flag.string ~name:"sort-by" ~default:""
      ~doc:"JSONPath expression to sort list types by" ()
  let subresource =
    Flag.string ~name:"subresource" ~default:""
      ~doc:"gets the subresource of the requested object (status|scale)" ()
  let template =
    Flag.string ~name:"template" ~default:""
      ~doc:"template string or path for -o=go-template[-file]" ()
  let watch =
    Flag.bool ~name:"watch" ~short:'w' ~default:false
      ~doc:"after listing, watch for changes" ()
  let watch_only =
    Flag.bool ~name:"watch-only" ~default:false
      ~doc:"watch for changes without an initial listing" ()

  let flags =
    [ Flag.pack all_namespaces
    ; Flag.pack allow_missing_template_keys
    ; Flag.pack chunk_size
    ; Flag.pack filename
    ; Flag.pack field_selector
    ; Flag.pack ignore_not_found
    ; Flag.pack kustomize
    ; Flag.pack label_columns
    ; Flag.pack no_headers
    ; Flag.pack output
    ; Flag.pack output_watch_events
    ; Flag.pack raw
    ; Flag.pack recursive
    ; Flag.pack selector
    ; Flag.pack server_print
    ; Flag.pack show_kind
    ; Flag.pack show_labels
    ; Flag.pack show_managed_fields
    ; Flag.pack sort_by
    ; Flag.pack subresource
    ; Flag.pack template
    ; Flag.pack watch
    ; Flag.pack watch_only
    ]

  let cmd =
    Command.make ~name:"get"
      ~short:"Display one or many resources"
      ~long:"Display one or many resources. Prints a table of the most important \
             information about the specified resources."
      ~group_id:"basic-intermediate"
      ~args:Arg.any   (* kubectl get accepts TYPE[/NAME] positionals *)
      ~flags ~run:stub ()
end

(* ============================================================ *)
(* kubectl delete                                                *)
(* ============================================================ *)

module Delete = struct
  let all =
    Flag.bool ~name:"all" ~default:false
      ~doc:"delete all resources in the namespace of the specified types" ()
  let all_namespaces =
    Flag.bool ~name:"all-namespaces" ~short:'A' ~default:false ~doc:"" ()
  let cascade =
    Flag.string ~name:"cascade" ~default:"background"
      ~doc:"deletion cascading strategy (background|orphan|foreground)" ()
  let dry_run =
    Flag.string ~name:"dry-run" ~default:"none"
      ~doc:"dry-run strategy (none|server|client)" ()
  let filename =
    Flag.repeated
      (Flag.string ~name:"filename" ~short:'f'
         ~doc:"file containing the resource to delete" ())
  let field_selector =
    Flag.string ~name:"field-selector" ~default:"" ~doc:"" ()
  let force =
    Flag.bool ~name:"force" ~default:false
      ~doc:"immediately remove resources and bypass graceful deletion" ()
  let grace_period =
    Flag.int ~name:"grace-period" ~default:(-1)
      ~doc:"seconds given to the resource to terminate gracefully" ()
  let ignore_not_found =
    Flag.bool ~name:"ignore-not-found" ~default:false
      ~doc:"treat 'not found' as a successful delete" ()
  let interactive =
    Flag.bool ~name:"interactive" ~short:'i' ~default:false
      ~doc:"delete only when the user confirms" ()
  let kustomize =
    Flag.string ~name:"kustomize" ~short:'k' ~default:""
      ~doc:"process a kustomization directory" ()
  let now =
    Flag.bool ~name:"now" ~default:false
      ~doc:"signal for immediate shutdown (same as --grace-period=1)" ()
  let output =
    Flag.string ~name:"output" ~short:'o' ~default:"" ~doc:"" ()
  let raw =
    Flag.string ~name:"raw" ~default:""
      ~doc:"raw URI to DELETE to the server" ()
  let recursive =
    Flag.bool ~name:"recursive" ~short:'R' ~default:false ~doc:"" ()
  let selector =
    Flag.string ~name:"selector" ~short:'l' ~default:"" ~doc:"" ()
  let timeout =
    Flag.string ~name:"timeout" ~default:"0s"
      ~doc:"length of time to wait before giving up" ()
  let wait =
    Flag.bool ~name:"wait" ~default:true
      ~doc:"wait for resources to be gone before returning" ()

  let flags =
    [ Flag.pack all
    ; Flag.pack all_namespaces
    ; Flag.pack cascade
    ; Flag.pack dry_run
    ; Flag.pack filename
    ; Flag.pack field_selector
    ; Flag.pack force
    ; Flag.pack grace_period
    ; Flag.pack ignore_not_found
    ; Flag.pack interactive
    ; Flag.pack kustomize
    ; Flag.pack now
    ; Flag.pack output
    ; Flag.pack raw
    ; Flag.pack recursive
    ; Flag.pack selector
    ; Flag.pack timeout
    ; Flag.pack wait
    ]

  let cmd =
    Command.make ~name:"delete"
      ~short:"Delete resources by file names, stdin, resources and names, or by resources and label selector"
      ~group_id:"basic-intermediate"
      ~args:Arg.any   (* delete accepts TYPE[/NAME] positionals *)
      ~flags ~run:stub ()
end

(* ============================================================ *)
(* kubectl config + sub-subcommands                              *)
(* ============================================================ *)

module Config = struct
  let leaf ~name ~short = Command.make ~name ~short ~run:stub ()

  let subs =
    [ leaf ~name:"current-context"  ~short:"Display the current-context"
    ; leaf ~name:"delete-cluster"   ~short:"Delete the specified cluster from the kubeconfig"
    ; leaf ~name:"delete-context"   ~short:"Delete the specified context from the kubeconfig"
    ; leaf ~name:"delete-user"      ~short:"Delete the specified user from the kubeconfig"
    ; leaf ~name:"get-clusters"     ~short:"Display clusters defined in the kubeconfig"
    ; leaf ~name:"get-contexts"     ~short:"Describe one or many contexts"
    ; leaf ~name:"get-users"        ~short:"Display users defined in the kubeconfig"
    ; leaf ~name:"rename-context"   ~short:"Rename a context from the kubeconfig file"
    ; leaf ~name:"set"              ~short:"Set an individual value in a kubeconfig file"
    ; leaf ~name:"set-cluster"      ~short:"Set a cluster entry in kubeconfig"
    ; leaf ~name:"set-context"      ~short:"Set a context entry in kubeconfig"
    ; leaf ~name:"set-credentials"  ~short:"Set a user entry in kubeconfig"
    ; leaf ~name:"unset"            ~short:"Unset an individual value in a kubeconfig file"
    ; leaf ~name:"use-context"      ~short:"Set the current-context in a kubeconfig file"
    ; leaf ~name:"view"             ~short:"Display merged kubeconfig settings" ]

  let cmd =
    Command.make ~name:"config"
      ~short:"Modify kubeconfig files"
      ~group_id:"other"
      ~subcommands:subs ()
end

(* ============================================================ *)
(* Other top-level commands (name + short description only)      *)
(* ============================================================ *)

let leaf ?group_id ~name ~short () = Command.make ~name ?group_id ~short ~run:stub ()

(* Mirrors kubectl --help's 8 sections (kubectl v1.31). *)
let kubectl_groups : (string * string) list =
  [ "basic-beginner",     "Basic Commands (Beginner)"
  ; "basic-intermediate", "Basic Commands (Intermediate)"
  ; "deploy",             "Deploy Commands"
  ; "cluster-mgmt",       "Cluster Management Commands"
  ; "troubleshoot",       "Troubleshooting and Debugging Commands"
  ; "advanced",           "Advanced Commands"
  ; "settings",           "Settings Commands"
  ; "other",              "Other Commands"
  ]

let other_top_level =
  [ (* Basic (Beginner) *)
    leaf ~group_id:"basic-beginner" ~name:"create"        ~short:"Create a resource from a file or from stdin" ()
  ; leaf ~group_id:"basic-beginner" ~name:"expose"        ~short:"Take a replication controller, service, deployment or pod and expose it as a new Kubernetes service" ()
  ; leaf ~group_id:"basic-beginner" ~name:"run"           ~short:"Run a particular image on the cluster" ()
  ; leaf ~group_id:"basic-beginner" ~name:"set"           ~short:"Set specific features on objects" ()
    (* Basic (Intermediate) -- get / delete are detailed above *)
  ; leaf ~group_id:"basic-intermediate" ~name:"explain"   ~short:"Get documentation for a resource" ()
  ; leaf ~group_id:"basic-intermediate" ~name:"edit"      ~short:"Edit a resource on the server" ()
    (* Deploy *)
  ; leaf ~group_id:"deploy" ~name:"rollout"       ~short:"Manage the rollout of a resource" ()
  ; leaf ~group_id:"deploy" ~name:"scale"         ~short:"Set a new size for a deployment, replica set, or replication controller" ()
  ; leaf ~group_id:"deploy" ~name:"autoscale"     ~short:"Auto-scale a deployment, replica set, stateful set, or replication controller" ()
    (* Cluster Management *)
  ; leaf ~group_id:"cluster-mgmt" ~name:"certificate"   ~short:"Modify certificate resources" ()
  ; leaf ~group_id:"cluster-mgmt" ~name:"cluster-info"  ~short:"Display cluster information" ()
  ; leaf ~group_id:"cluster-mgmt" ~name:"top"           ~short:"Display resource (CPU/memory) usage" ()
  ; leaf ~group_id:"cluster-mgmt" ~name:"cordon"        ~short:"Mark node as unschedulable" ()
  ; leaf ~group_id:"cluster-mgmt" ~name:"uncordon"      ~short:"Mark node as schedulable" ()
  ; leaf ~group_id:"cluster-mgmt" ~name:"drain"         ~short:"Drain node in preparation for maintenance" ()
  ; leaf ~group_id:"cluster-mgmt" ~name:"taint"         ~short:"Update the taints on one or more nodes" ()
    (* Troubleshooting *)
  ; leaf ~group_id:"troubleshoot" ~name:"describe"      ~short:"Show details of a specific resource or group of resources" ()
  ; leaf ~group_id:"troubleshoot" ~name:"logs"          ~short:"Print the logs for a container in a pod" ()
  ; leaf ~group_id:"troubleshoot" ~name:"attach"        ~short:"Attach to a running container" ()
  ; leaf ~group_id:"troubleshoot" ~name:"exec"          ~short:"Execute a command in a container" ()
  ; leaf ~group_id:"troubleshoot" ~name:"port-forward"  ~short:"Forward one or more local ports to a pod" ()
  ; leaf ~group_id:"troubleshoot" ~name:"proxy"         ~short:"Run a proxy to the Kubernetes API server" ()
  ; leaf ~group_id:"troubleshoot" ~name:"cp"            ~short:"Copy files and directories to and from containers" ()
  ; leaf ~group_id:"troubleshoot" ~name:"auth"          ~short:"Inspect authorization" ()
  ; leaf ~group_id:"troubleshoot" ~name:"debug"         ~short:"Create debugging sessions for troubleshooting workloads and nodes" ()
  ; leaf ~group_id:"troubleshoot" ~name:"events"        ~short:"List events" ()
    (* Advanced *)
  ; leaf ~group_id:"advanced" ~name:"diff"          ~short:"Diff the live version against a would-be applied version" ()
  ; leaf ~group_id:"advanced" ~name:"apply"         ~short:"Apply a configuration to a resource by file name or stdin" ()
  ; leaf ~group_id:"advanced" ~name:"patch"         ~short:"Update fields of a resource" ()
  ; leaf ~group_id:"advanced" ~name:"replace"       ~short:"Replace a resource by file name or stdin" ()
  ; leaf ~group_id:"advanced" ~name:"wait"          ~short:"Experimental: Wait for a specific condition on one or many resources" ()
  ; leaf ~group_id:"advanced" ~name:"kustomize"     ~short:"Build a kustomization target from a directory or URL" ()
    (* Settings *)
  ; leaf ~group_id:"settings" ~name:"label"         ~short:"Update the labels on a resource" ()
  ; leaf ~group_id:"settings" ~name:"annotate"      ~short:"Update the annotations on a resource" ()
    (* Other *)
  ; leaf ~group_id:"other" ~name:"api-resources" ~short:"Print the supported API resources on the server" ()
  ; leaf ~group_id:"other" ~name:"api-versions"  ~short:"Print the supported API versions on the server, in the form of \"group/version\"" ()
  ; leaf ~group_id:"other" ~name:"plugin"        ~short:"Provides utilities for interacting with plugins" ()
  ; leaf ~group_id:"other" ~name:"version"       ~short:"Print the client and server version information" ()
  ]

let root =
  Command.make ~name:"kubectl"
    ~short:"kubectl controls the Kubernetes cluster manager"
    ~long:"kubectl controls the Kubernetes cluster manager.\n\
           \n\
           Find more information at: https://kubernetes.io/docs/reference/kubectl/"
    ~persistent_flags:global_flags
    ~groups:kubectl_groups
    ~subcommands:(Get.cmd :: Delete.cmd :: Config.cmd :: other_top_level)
    ()

(* ============================================================ *)
(* Test infrastructure                                           *)
(* ============================================================ *)

let exec args =
  let out_buf = Buffer.create 256 in
  let err_buf = Buffer.create 256 in
  let prog =
    Program.make ~name:"kubectl" ~version:"v1.31.0"
      ~root
      ~help_command:true ~completion_command:false
      ~out:(Format.formatter_of_buffer out_buf)
      ~err:(Format.formatter_of_buffer err_buf)
      ()
  in
  let argv = Array.of_list ("kubectl" :: args) in
  let code = Program.run prog ~argv in
  (code, Buffer.contents out_buf, Buffer.contents err_buf)

let dispatch args =
  let prog =
    Program.make ~name:"kubectl" ~version:"v1.31.0" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer (Buffer.create 16))
      ~err:(Format.formatter_of_buffer (Buffer.create 16))
      ()
  in
  Program.dispatch prog ~argv:(Array.of_list ("kubectl" :: args))

(* ============================================================ *)
(* Tests                                                         *)
(* ============================================================ *)

(* Every top-level command name kubectl prints in `kubectl --help` should
   be expressible/dispatch-able under mamba's tree. *)
let kubectl_top_level_names = [
  (* Basic Commands (Beginner) *)
  "create"; "expose"; "run"; "set";
  (* Basic Commands (Intermediate) *)
  "explain"; "get"; "edit"; "delete";
  (* Deploy Commands *)
  "rollout"; "scale"; "autoscale";
  (* Cluster Management Commands *)
  "certificate"; "cluster-info"; "top"; "cordon"; "uncordon"; "drain"; "taint";
  (* Troubleshooting and Debugging Commands *)
  "describe"; "logs"; "attach"; "exec"; "port-forward"; "proxy"; "cp"; "auth"; "debug"; "events";
  (* Advanced Commands *)
  "diff"; "apply"; "patch"; "replace"; "wait"; "kustomize";
  (* Settings Commands *)
  "label"; "annotate";
  (* Other Commands *)
  "api-resources"; "api-versions"; "config"; "plugin"; "version";
]

let test_dispatch_top_level name () =
  match dispatch [ name ] with
  | Run { command; _ } ->
    Alcotest.(check string) "dispatched cmd name" name command.name
  | _ -> Alcotest.fail (Printf.sprintf "dispatch %s did not Run" name)

let test_global_flag_inherits_to_get () =
  match dispatch [ "--namespace"; "kube-system"; "get"; "pods" ] with
  | Run { command; args; _ } ->
    Alcotest.(check string) "leaf is get" "get" command.name;
    Alcotest.(check string) "namespace inherited" "kube-system"
      (Args.get args g_namespace);
    Alcotest.(check (list string)) "positional" [ "pods" ]
      (Args.positional args)
  | _ -> Alcotest.fail "expected Run"

let test_global_flag_short_inherits () =
  match dispatch [ "-n"; "kube-system"; "delete"; "pod"; "x" ] with
  | Run { command; args; _ } ->
    Alcotest.(check string) "leaf is delete" "delete" command.name;
    Alcotest.(check string) "namespace inherited" "kube-system"
      (Args.get args g_namespace)
  | _ -> Alcotest.fail "expected Run"

let test_get_flags_parse () =
  match dispatch
    [ "get"; "pods"; "-A"; "-o"; "yaml"; "-l"; "app=nginx"; "--watch" ]
  with
  | Run { command; args; _ } ->
    Alcotest.(check string) "get" "get" command.name;
    Alcotest.(check bool)   "-A"          true (Args.get args Get.all_namespaces);
    Alcotest.(check string) "-o yaml"     "yaml" (Args.get args Get.output);
    Alcotest.(check string) "-l selector" "app=nginx" (Args.get args Get.selector);
    Alcotest.(check bool)   "--watch"     true (Args.get args Get.watch);
    Alcotest.(check (list string)) "positionals" [ "pods" ]
      (Args.positional args)
  | _ -> Alcotest.fail "expected Run"

let test_delete_flags_parse () =
  match dispatch
    [ "delete"; "pod"; "x"; "--grace-period"; "30"; "--force"; "--wait=false" ]
  with
  | Run { command; args; _ } ->
    Alcotest.(check string) "delete" "delete" command.name;
    Alcotest.(check int)    "grace-period" 30 (Args.get args Delete.grace_period);
    Alcotest.(check bool)   "force"       true  (Args.get args Delete.force);
    Alcotest.(check bool)   "wait=false"  false (Args.get args Delete.wait);
    Alcotest.(check (list string)) "positionals" [ "pod"; "x" ]
      (Args.positional args)
  | _ -> Alcotest.fail "expected Run"

let test_nested_config_dispatch () =
  match dispatch [ "config"; "use-context"; "minikube" ] with
  | Run { command; args; _ } ->
    Alcotest.(check string) "leaf" "use-context" command.name;
    Alcotest.(check (list string)) "positionals" [ "minikube" ]
      (Args.positional args)
  | _ -> Alcotest.fail "expected Run"

let test_unknown_command_suggests () =
  (* kubectl typo "got" should suggest "get". With max_distance=3, mamba
     finds "get" within distance 1 (insert o between g and t? No -- 'got'
     vs 'get' is sub 'o'→'e', distance 1). *)
  let _code, _out, err = exec [ "got"; "pods" ] in
  Alcotest.(check bool) "Did-you-mean fires" true
    (contains err {|Did you mean "get"|})

let test_unknown_flag_errors () =
  let code, _, _ = exec [ "get"; "pods"; "--definitely-not-a-flag" ] in
  Alcotest.(check int) "exit 2" Error.parse_error code

(* Parity: every flag name that kubectl `get --help` advertises is also
   present in mamba's Get module declaration. The list is hand-derived
   from tests/kubectl_ref/help_get.txt. *)
let kubectl_get_flag_names = [
  "all-namespaces"; "allow-missing-template-keys"; "chunk-size"; "filename";
  "field-selector"; "ignore-not-found"; "kustomize"; "label-columns";
  "no-headers"; "output"; "output-watch-events"; "raw"; "recursive";
  "selector"; "server-print"; "show-kind"; "show-labels";
  "show-managed-fields"; "sort-by"; "subresource"; "template"; "watch";
  "watch-only";
]

let mamba_get_flag_names =
  List.map (fun (Flag.P f) -> Flag.name f) Get.flags

let test_get_flag_parity () =
  let missing =
    List.filter (fun n -> not (List.mem n mamba_get_flag_names))
      kubectl_get_flag_names
  in
  Alcotest.(check (list string)) "no kubectl-get flags missing in mamba"
    [] missing

let kubectl_delete_flag_names = [
  "all"; "all-namespaces"; "cascade"; "dry-run"; "filename"; "field-selector";
  "force"; "grace-period"; "ignore-not-found"; "interactive"; "kustomize";
  "now"; "output"; "raw"; "recursive"; "selector"; "timeout"; "wait";
]

let mamba_delete_flag_names =
  List.map (fun (Flag.P f) -> Flag.name f) Delete.flags

let test_delete_flag_parity () =
  let missing =
    List.filter (fun n -> not (List.mem n mamba_delete_flag_names))
      kubectl_delete_flag_names
  in
  Alcotest.(check (list string)) "no kubectl-delete flags missing in mamba"
    [] missing

(* `help <command>` must show that command's description. *)
let test_help_for_get () =
  let _code, out, _ = exec [ "help"; "get" ] in
  Alcotest.(check bool) "help mentions get's description" true
    (contains out "Display one or many resources")

let test_help_for_root () =
  let _code, out, _ = exec [ "--help" ] in
  Alcotest.(check bool) "help mentions tagline" true
    (contains out "kubectl controls the Kubernetes cluster manager")

(* Every kubectl group title should appear in --help, matching kubectl's
   own section headings. *)
let test_help_lists_all_groups () =
  let _code, out, _ = exec [ "--help" ] in
  List.iter
    (fun (_id, title) ->
       Alcotest.(check bool) ("contains " ^ title) true (contains out title))
    kubectl_groups

(* A representative command from each group should appear after the group's
   header (cheap structural check: title appears, then command name appears
   later in the buffer). *)
let after str needle1 needle2 =
  match String.index_opt str needle1.[0], String.index_opt str needle2.[0] with
  | _ -> contains str needle1 && contains str needle2 &&
         (* find rough positions *)
         (let rec find_at sub from =
            if from + String.length sub > String.length str then -1
            else if String.sub str from (String.length sub) = sub then from
            else find_at sub (from + 1)
          in
          find_at needle1 0 < find_at needle2 (find_at needle1 0))

let test_help_get_under_basic_intermediate () =
  let _code, out, _ = exec [ "--help" ] in
  Alcotest.(check bool) "get listed after its group header" true
    (after out "Basic Commands (Intermediate)" "get")

(* kubectl-parity: repeated -f occurrences accumulate into a list, matching
   pflag's StringArray behavior. *)
let test_repeated_filename () =
  match dispatch [ "delete"; "-f"; "a.yaml"; "-f"; "b.yaml" ] with
  | Run { args; _ } ->
    Alcotest.(check (list string)) "all -f accumulate"
      [ "a.yaml"; "b.yaml" ] (Args.get args Delete.filename)
  | _ -> Alcotest.fail "expected Run"

(* Same shape with --filename=value and the long form intermixed with short. *)
let test_repeated_filename_mixed () =
  match dispatch
    [ "delete"; "-f"; "a.yaml"; "--filename"; "b.yaml"; "--filename=c.yaml" ] with
  | Run { args; _ } ->
    Alcotest.(check (list string)) "short + long both contribute"
      [ "a.yaml"; "b.yaml"; "c.yaml" ] (Args.get args Delete.filename)
  | _ -> Alcotest.fail "expected Run"

(* ============================================================ *)
(* Runner                                                        *)
(* ============================================================ *)

let tc name f = Alcotest.test_case name `Quick f

let () =
  let top_level_cases =
    List.map
      (fun n -> tc ("top-level: " ^ n) (test_dispatch_top_level n))
      kubectl_top_level_names
  in
  Alcotest.run "kubectl_parity"
    [
      "TopLevelDispatch", top_level_cases;
      "GlobalFlags",
      [ tc "long --namespace inherits to get" test_global_flag_inherits_to_get
      ; tc "short -n inherits to delete"      test_global_flag_short_inherits
      ];
      "GetFlags",
      [ tc "common argv parses"  test_get_flags_parse
      ; tc "parity with kubectl" test_get_flag_parity
      ];
      "DeleteFlags",
      [ tc "common argv parses"  test_delete_flags_parse
      ; tc "parity with kubectl" test_delete_flag_parity
      ];
      "Nested",
      [ tc "config use-context"  test_nested_config_dispatch
      ];
      "Errors",
      [ tc "unknown command suggests" test_unknown_command_suggests
      ; tc "unknown flag exits 2"     test_unknown_flag_errors
      ];
      "Help",
      [ tc "help for get"                       test_help_for_get
      ; tc "help for root"                      test_help_for_root
      ; tc "help lists every group title"       test_help_lists_all_groups
      ; tc "get appears under its group header" test_help_get_under_basic_intermediate
      ];
      "Repeated",
      [ tc "delete -f a -f b -> [a; b]"               test_repeated_filename
      ; tc "delete mixed -f + --filename -> all kept" test_repeated_filename_mixed
      ];
    ]
