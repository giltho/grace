open! Core
open! Grace
open Diagnostic

let source name content : Source.t =
  `String { name = Some name; content = Dedent.string content }
;;

let range ~source start stop =
  Range.create ~source (Byte_index.of_int start) (Byte_index.of_int stop)
;;

let pr_diagnostics diagnostics =
  let open Grace_ansi_renderer in
  (* Disable colors for tests (since expect tests don't support ANSI colors) *)
  let config = Config.{ default with use_ansi = Some false } in
  Fmt.(
    list
      ~sep:(fun ppf () -> pf ppf "@.@.@.")
      (fun ppf diagnostic ->
         pp_diagnostic ~config ppf diagnostic;
         pf ppf "@.@.";
         pp_compact_diagnostic ~config ppf diagnostic))
    Fmt.stdout
    diagnostics
;;

let pr_bad_diagnostics diagnostics =
  let open Grace_ansi_renderer in
  let config = Config.{ default with use_ansi = Some false } in
  Fmt.(
    list
      ~sep:(fun ppf () -> pf ppf "@.@.")
      (fun ppf diagnostic ->
         try pp_diagnostic ~config ppf diagnostic with
         | exn -> Fmt.pf ppf "Raised: %s" (Exn.to_string exn)))
    Fmt.stdout
    diagnostics
;;

(* Taken from https://github.com/brendanzab/codespan/blob/master/codespan-reporting/tests/term.rs *)

let%expect_test "empty" =
  let diagnostics =
    let empty severity =
      Diagnostic.
        { severity
        ; message = (fun ppf -> Fmt.pf ppf "")
        ; labels = []
        ; notes = []
        ; code = None
        }
    in
    List.map ~f:empty Severity.[ Help; Note; Warning; Error; Bug ]
  in
  pr_diagnostics diagnostics;
  [%expect
    {|
    help:

    help:


    note:

    note:


    warning:

    warning:


    error:

    error:


    bug:

    bug: |}]
;;

let%expect_test "same_line" =
  let source =
    source
      "one_line.rs"
      {|  
      > fn main() {
      >     let mut v = vec![Some("foo"), Some("bar")];
      >     v.push(v.pop().unwrap());
      > }
      |}
  in
  let diagnostics =
    [ Diagnostic.createf
        ~labels:
          [ Label.primaryf
              ~range:(range ~source 71 72)
              "second mutable borrow occurs here"
          ; Label.secondaryf
              ~range:(range ~source 64 65)
              "first borrow later used by call"
          ; Label.secondaryf
              ~range:(range ~source 66 70)
              "first mutable borrow occurs here"
          ]
        Error
        "cannot borrow `v` as mutable more than once at a time"
    ; Diagnostic.createf
        ~notes:
          [ Message.create "For more information about this error, try `rustc --explain`"
          ]
        Error
        "aborting due to previous error"
    ]
  in
  pr_diagnostics diagnostics;
  [%expect
    {|
    error: cannot borrow `v` as mutable more than once at a time
        ┌─ one_line.rs:3:12
      3 │      v.push(v.pop().unwrap());
        │      - ---- ^ second mutable borrow occurs here
        │      │ │
        │      │ first mutable borrow occurs here
        │      first borrow later used by call

    one_line.rs:3:12: error: cannot borrow `v` as mutable more than once at a time


    error: aborting due to previous error
        = For more information about this error, try `rustc --explain`

    error: aborting due to previous error
     = For more information about this error, try `rustc --explain`
    |}]
;;

let%expect_test "overlapping" =
  let s1 =
    source
      "nested_impl_trait.rs"
      {|
      > use std::fmt::Debug;
      >
      > fn fine(x: impl Into<u32>) -> impl Into<u32> { x }
      >
      > fn bad_in_ret_position(x: impl Into<u32>) -> impl Into<impl Debug> { x }
      |}
  in
  let s2 =
    source
      "typeck_type_placeholder_item.rs"
      {|
      > fn fn_test1() -> _ { 5 }
      > fn fn_test2(x: i32) -> (_, _) { (x, x) }  
      |}
  in
  let s3 =
    source
      "libstd/thread/mod.rs"
      {|
      > #[stable(feature = "rust1", since = "1.0.0")]
      > pub fn spawn<F, T>(self, f: F) -> io::Result<JoinHandle<T>>
      > where
      >     F: FnOnce() -> T,
      >     F: Send + 'static,
      >     T: Send + 'static,
      > {
      >  unsafe { self.spawn_unchecked(f) }
      > }
      |}
  in
  let s4 =
    source
      "no_send_res_ports.rs"
      {|
      > use std::thread;
      > use std::rc::Rc;
      >
      > #[derive(Debug)]
      > struct Port<T>(Rc<T>);
      >
      > fn main() {
      >     #[derive(Debug)]
      >     struct Foo {
      >         _x: Port<()>,
      >     }
      >
      >     impl Drop for Foo {
      >         fn drop(&mut self) {}
      >     }
      >
      >     fn foo(x: Port<()>) -> Foo {
      >         Foo {
      >             _x: x
      >         }
      >     }
      >
      >     let x = foo(Port(Rc::new(())));
      >
      >     thread::spawn(move|| {
      >         let y = x;
      >         println!("{:?}", y);
      >     });
      > }
      |}
  in
  let diagnostics =
    Diagnostic.
      [ createf
          ~labels:
            Label.
              [ primaryf ~range:(range ~source:s1 129 139) "nested `impl Trait` here"
              ; secondaryf ~range:(range ~source:s1 119 140) "outer `impl Trait`"
              ]
          Error
          "nested `impl Trait` is not allowed"
      ; createf
          ~labels:
            Label.
              [ primaryf ~range:(range ~source:s2 17 18) "not allowed in type signatures"
              ; secondaryf
                  ~range:(range ~source:s2 17 18)
                  "help: replace with the correct return type: `i32`"
              ]
          Error
          "the type placeholder `_` is not allowed within types on item signatures"
      ; createf
          ~labels:
            Label.
              [ primaryf ~range:(range ~source:s2 49 50) "not allowed in type signatures"
              ; primaryf ~range:(range ~source:s2 52 53) "not allowed in type signatures"
              ; secondaryf
                  ~range:(range ~source:s2 48 54)
                  "help: replace with the correct return type: `(i32, i32)`"
              ]
          Error
          "the type placeholder `_` is not allowed within types on item signatures"
      ; createf
          ~labels:
            Label.
              [ primaryf
                  ~range:(range ~source:s4 339 352)
                  "`std::rc::Rc<()> cannot be sent between threads safely`"
              ; secondaryf
                  ~range:(range ~source:s4 353 416)
                  "within this `[closure@no_send_res_ports.rs:29:19: 33:6 x:main::Foo]`"
              ; secondaryf
                  ~range:(range ~source:s3 141 145)
                  "required by this bound in `std::thread::spawn`"
              ]
          ~notes:
            Message.
              [ create
                  "help: within `[closure@no_send_res_ports.rs:29:19: 33:6 \
                   x:main::Foo]`, the trait `std::marker::Send` is not implemented for \
                   `std::rc::Rc<()>`"
              ; create "note: required because it appears within the type `Port<()>`"
              ; create "note: required because it appears within the type `main::Foo`"
              ; create
                  "note: required because it appears within the type \
                   `[closure@no_send_res_ports.rs:29:19: 33:6 x:main::Foo]`"
              ]
          Error
          "`std::rc::Rc<()>` cannot be sent between threads safely"
      ; createf
          ~notes:
            Message.
              [ create "Some errors have detailed explanations: ..."
              ; create "For more information about an error, try `rustc --explain`"
              ]
          Error
          "aborting due 5 previous errors"
      ]
  in
  pr_diagnostics diagnostics;
  [%expect
    {|
    error: nested `impl Trait` is not allowed
        ┌─ nested_impl_trait.rs:5:56
      5 │  fn bad_in_ret_position(x: impl Into<u32>) -> impl Into<impl Debug> { x }
        │                                               ----------^^^^^^^^^^-
        │                                               │         │
        │                                               │         nested `impl Trait` here
        │                                               outer `impl Trait`

    nested_impl_trait.rs:5:56: error: nested `impl Trait` is not allowed


    error: the type placeholder `_` is not allowed within types on item signatures
        ┌─ typeck_type_placeholder_item.rs:1:18
      1 │  fn fn_test1() -> _ { 5 }
        │                   ^
        │                   │
        │                   not allowed in type signatures
        │                   help: replace with the correct return type: `i32`

    typeck_type_placeholder_item.rs:1:18: error: the type placeholder `_` is not allowed within types on item signatures


    error: the type placeholder `_` is not allowed within types on item signatures
        ┌─ typeck_type_placeholder_item.rs:2:28
      2 │  fn fn_test2(x: i32) -> (_, _) { (x, x) }
        │                         -^--^-
        │                         ││  │
        │                         ││  not allowed in type signatures
        │                         │not allowed in type signatures
        │                         help: replace with the correct return type: `(i32, i32)`

    typeck_type_placeholder_item.rs:2:28: error: the type placeholder `_` is not allowed within types on item signatures


    error: `std::rc::Rc<()>` cannot be sent between threads safely
        ┌─ libstd/thread/mod.rs:5:8
      5 │        F: Send + 'static,
        │           ---- required by this bound in `std::thread::spawn`
        ┌─ no_send_res_ports.rs:25:5
     24 │
     25 │        thread::spawn(move|| {
        │        ^^^^^^^^^^^^^ `std::rc::Rc<()> cannot be sent between threads safely`
        │ ╭────────────────────'
     26 │ │          let y = x;
     27 │ │          println!("{:?}", y);
     28 │ │      });
        │ ╰───────' within this `[closure@no_send_res_ports.rs:29:19: 33:6 x:main::Foo]`
     29 │    }
        = help: within `[closure@no_send_res_ports.rs:29:19: 33:6 x:main::Foo]`, the trait `std::marker::Send` is not implemented for `std::rc::Rc<()>`
        = note: required because it appears within the type `Port<()>`
        = note: required because it appears within the type `main::Foo`
        = note: required because it appears within the type `[closure@no_send_res_ports.rs:29:19: 33:6 x:main::Foo]`

    libstd/thread/mod.rs:5:8: error: `std::rc::Rc<()>` cannot be sent between threads safely
    no_send_res_ports.rs:25:5: error: `std::rc::Rc<()>` cannot be sent between threads safely
     = help: within `[closure@no_send_res_ports.rs:29:19: 33:6 x:main::Foo]`, the trait `std::marker::Send` is not implemented for `std::rc::Rc<()>`
     = note: required because it appears within the type `Port<()>`
     = note: required because it appears within the type `main::Foo`
     = note: required because it appears within the type `[closure@no_send_res_ports.rs:29:19: 33:6 x:main::Foo]`


    error: aborting due 5 previous errors
        = Some errors have detailed explanations: ...
        = For more information about an error, try `rustc --explain`

    error: aborting due 5 previous errors
     = Some errors have detailed explanations: ...
     = For more information about an error, try `rustc --explain`
    |}]
;;

let%expect_test "same ranges" =
  let source = source "same_range" "::S { }" in
  let diagnostics =
    Diagnostic.
      [ createf
          ~labels:
            Label.
              [ primaryf ~range:(range ~source 4 5) "Unexpected '{'"
              ; secondaryf ~range:(range ~source 4 5) "Expected '('"
              ]
          Error
          "unexpected token"
      ]
  in
  pr_diagnostics diagnostics;
  [%expect
    {|
    error: unexpected token
        ┌─ same_range:1:5
      1 │  ::S { }
        │      ^
        │      │
        │      Unexpected '{'
        │      Expected '('

    same_range:1:5: error: unexpected token
    |}]
;;

let%expect_test "multiline_overlapping" =
  let source =
    source
      "file.rs"
      {|
      >         match line_index.compare(self.last_line_index()) {
      >             Ordering::Less => Ok(self.line_starts()[line_index.to_usize()]),
      >             Ordering::Equal => Ok(self.source_span().end()),
      >             Ordering::Greater => LineIndexOutOfBoundsError {
      >                 given: line_index,
      >                 max: self.last_line_index()
      >             },
      >         }
      |}
  in
  let diagnostics =
    Diagnostic.
      [ createf
          ~notes:
            [ Message.create
                "expected `Result<ByteIndex, LineIndexOutOfBoundsError>`, found \
                 `LineIndexOutOfBoundsError`"
            ]
          ~labels:
            Label.
              [ secondaryf
                  ~range:(range ~source 89 134)
                  "this is found to be of type `Result<ByteIndex, \
                   LineIndexOutOfBoundsError>`"
              ; primaryf
                  ~range:(range ~source 230 350)
                  "expected enum `Result`, found struct `LineIndexOutOfBoundsError`"
              ; secondaryf
                  ~range:(range ~source 8 361)
                  "`match` arms have incompatible types"
              ; secondaryf
                  ~range:(range ~source 167 195)
                  "this is found to be of type `Result<ByteIndex, \
                   LineIndexOutOfBoundsError>`"
              ]
          Error
          "match arms have incompatible types"
      ]
  in
  pr_diagnostics diagnostics;
  [%expect
    {|
    error: match arms have incompatible types
        ┌─ file.rs:4:34
      1 │ ╭            match line_index.compare(self.last_line_index()) {
      2 │ │                Ordering::Less => Ok(self.line_starts()[line_index.to_usize()]),
        │ │                                  --------------------------------------------- this is found to be of type `Result<ByteIndex, LineIndexOutOfBoundsError>`
      3 │ │                Ordering::Equal => Ok(self.source_span().end()),
        │ │                                   ---------------------------- this is found to be of type `Result<ByteIndex, LineIndexOutOfBoundsError>`
      4 │ │                Ordering::Greater => LineIndexOutOfBoundsError {
        │ │ ╭───────────────────────────────────^
      5 │ │ │                  given: line_index,
      6 │ │ │                  max: self.last_line_index()
      7 │ │ │              },
        │ │ ╰──────────────^ expected enum `Result`, found struct `LineIndexOutOfBoundsError`
      8 │ │            }
        │ ╰────────────' `match` arms have incompatible types
        = expected `Result<ByteIndex, LineIndexOutOfBoundsError>`, found `LineIndexOutOfBoundsError`

    file.rs:4:34: error: match arms have incompatible types
     = expected `Result<ByteIndex, LineIndexOutOfBoundsError>`, found `LineIndexOutOfBoundsError`
    |}]
;;

let%expect_test "unicode" =
  let source = source "unicode.rs" {|extern "路濫狼á́́" fn foo() {}|} in
  let diagnostics =
    Diagnostic.
      [ createf
          ~notes:
            [ Message.createf
                "@[<v 3>valid ABIs:@.- aapcs@.- amdgpu-kernel@.- C@.- cdecl@.- efiapi@.- \
                 fastcall@.- msp430-interrupt@.- platform-intrinsic@.- ptx-kernel@.- \
                 Rust@.- rust-call@.- rust-intrinsic@.- stdcall@.- system@.- sysv64@.- \
                 thiscall@.- unadjusted@.- vectorcall@.- win64@.- x86-interrupt@]"
            ]
          ~labels:[ Label.primaryf ~range:(range ~source 7 24) "invalid ABI" ]
          Error
          "invalid ABI: found `路濫狼á́́`"
      ]
  in
  pr_diagnostics diagnostics;
  [%expect
    {|
    error: invalid ABI: found `路濫狼á́́`
        ┌─ unicode.rs:1:8
      1 │  extern "路濫狼á́́" fn foo() {}
        │         ^^^^^^^^ invalid ABI
        = valid ABIs:
          - aapcs
          - amdgpu-kernel
          - C
          - cdecl
          - efiapi
          - fastcall
          - msp430-interrupt
          - platform-intrinsic
          - ptx-kernel
          - Rust
          - rust-call
          - rust-intrinsic
          - stdcall
          - system
          - sysv64
          - thiscall
          - unadjusted
          - vectorcall
          - win64
          - x86-interrupt

    unicode.rs:1:8: error: invalid ABI: found `路濫狼á́́`
     = valid ABIs:
       - aapcs
       - amdgpu-kernel
       - C
       - cdecl
       - efiapi
       - fastcall
       - msp430-interrupt
       - platform-intrinsic
       - ptx-kernel
       - Rust
       - rust-call
       - rust-intrinsic
       - stdcall
       - system
       - sysv64
       - thiscall
       - unadjusted
       - vectorcall
       - win64
       - x86-interrupt |}]
;;

let%expect_test "unicode spans" =
  let source = source "moon_jump.rs" "🐄🌑🐄🌒🐄🌓🐄🌔🐄🌕🐄🌖🐄🌗🐄🌘🐄" in
  let invalid_start = 1 in
  let invalid_stop = String.length "🐄" - 1 in
  let diagnostics =
    Diagnostic.
      [ createf
          ~labels:
            [ Label.primaryf
                ~range:(range ~source invalid_start invalid_stop)
                "Invalid jump"
            ]
          Error
          "Cow may not jump during new moon."
      ; createf
          ~labels:
            [ Label.secondaryf
                ~range:(range ~source invalid_start (String.length "🐄"))
                "Cow range does not start at boundary."
            ]
          Note
          "Invalid unicode range"
      ; createf
          ~labels:
            [ Label.secondaryf
                ~range:(range ~source (String.length "🐄🌑") (String.length "🐄🌑🐄" - 1))
                "Cow range does not end at boundary"
            ]
          Note
          "Invalid unicode range"
      ; createf
          ~labels:
            [ Label.secondaryf
                ~range:(range ~source invalid_start (String.length "🐄🌑🐄" - 1))
                "Cow does not start or end at boundary."
            ]
          Note
          "Invalid unicode range"
      ]
  in
  pr_bad_diagnostics diagnostics;
  [%expect
    {|
    Raised: (Invalid_argument "invalid UTF-8")

    Raised: (Invalid_argument "invalid UTF-8")

    Raised: (Invalid_argument "invalid UTF-8")

    Raised: (Invalid_argument "invalid UTF-8") |}]
;;

let%expect_test "multi-line empty messages" =
  let source =
    source
      "rigid_variable_escape.ml"
      {|
      > let escape = fun f -> 
      >   fun (type a) -> 
      >     (f : a -> a)
      > ;;
      |}
  in
  let diagnostics =
    Diagnostic.
      [ createf
          ~labels:[ Label.primaryf ~range:(range ~source 24 56) "" ]
          Error
          "generic type variable `a` escapes its scope"
      ]
  in
  pr_diagnostics diagnostics;
  [%expect
    {|
    error: generic type variable `a` escapes its scope
        ┌─ rigid_variable_escape.ml:2:3
      1 │    let escape = fun f ->
      2 │ ╭    fun (type a) ->
      3 │ │      (f : a -> a)
        │ ╰─────────────────^
      4 │    ;;

    rigid_variable_escape.ml:2:3: error: generic type variable `a` escapes its scope
    |}]
;;

let%expect_test "multi-label on large file" =
  let source = source "large_files.rs" Large_fixture.large_file in
  let diagnostics =
    Diagnostic.
      [ createf
          ~labels:
            [ Label.primaryf ~range:(range ~source 2952 3014) "Error is happening here"
            ; Label.primaryf ~range:(range ~source 16313 16339) "Bananad from here"
            ]
          Error
          "Some dramatic error"
      ]
  in
  pr_diagnostics diagnostics;
  [%expect
    {|
    error: Some dramatic error
        ┌─ large_files.rs:515:21
    148 │           ~compare:(Comparable.pair Diagnostic.Priority.compare Byte_index.compare)
        │                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Error is happening here
    149 │      |> Option.get ~here:__LOC__
    150 │    in
    151 │    let line = Source_reader.Line.of_byte_index sd locus_idx in
    152 │    Line_number.of_line_index line.idx, Column_number.of_byte_index locus_idx ~sd ~line
    153 │  ;;
    154 │
    155 │  let group_labels_by_source labels =
    156 │    labels
    157 │    |> List.sort_and_group
    158 │         ~compare:
    159 │           (Comparable.lift Source.compare ~f:(fun (label : Label.t) ->
    160 │              Range.source label.range))
    161 │    |> List.map ~f:(fun labels ->
    162 │      (* Invariants:
    163 │         + [List.length labels > 0]
    164 │         + Sources for each label are equal *)
    165 │      let source = Range.source (List.hd_exn labels).Label.range in
    166 │      source, labels)
    167 │  ;;
    168 │
    169 │  module Of_diagnostic = struct
    170 │    type 'a with_line =
    171 │      { line : Line_index.t
    172 │      ; it : 'a
    173 │      }
    174 │
    175 │    let range_of_labels = function
    176 │      | [] -> assert false
    177 │      | { Label.range; _ } :: _ as labels ->
    178 │        List.fold_left
    179 │          labels
    180 │          ~f:(fun range label -> Range.merge range label.Label.range)
    181 │          ~init:range
    182 │    ;;
    183 │
    184 │    let line_of_range ~sd (line : Source_reader.Line.t) =
    185 │      let content = Source_reader.Line.slice ~sd line in
    186 │      { line = line.idx
    187 │      ; it =
    188 │          Line.
    189 │            { segments = [ { content; length = Utf8.length content; stag = None } ]
    190 │            ; multi_line_labels = []
    191 │            ; margin_length = margin_length_of_string content
    192 │            }
    193 │      }
    194 │    ;;
    195 │
    196 │    let line_of_idx ~sd (line_idx : Line_index.t) =
    197 │      line_of_range ~sd @@ Source_reader.Line.of_line_index sd line_idx
    198 │    ;;
    199 │
    200 │    module Priority_count = struct
    201 │      type t =
    202 │        { primary : int
    203 │        ; secondary : int
    204 │        }
    205 │
    206 │      let zero = { primary = 0; secondary = 0 }
    207 │
    208 │      let start t ~priority : t =
    209 │        match priority with
    210 │        | Priority.Primary -> { t with primary = t.primary + 1 }
    211 │        | Secondary -> { t with secondary = t.secondary + 1 }
    212 │      ;;
    213 │
    214 │      let stop t ~priority : t =
    215 │        match priority with
    216 │        | Priority.Primary ->
    217 │          assert (t.primary > 0);
    218 │          { t with primary = t.primary - 1 }
    219 │        | Secondary ->
    220 │          assert (t.secondary > 0);
    221 │          { t with secondary = t.secondary - 1 }
    222 │      ;;
    223 │
    224 │      let priority = function
    225 │        | { primary = 0; secondary = 0 } -> None
    226 │        | { primary = 0; secondary = _ } -> Some Priority.Secondary
    227 │        | { primary = _; secondary = _ } -> Some Primary
    228 │      ;;
    229 │    end
    230 │
    231 │    let add_eol_segment ~sd ~(line : Source_reader.Line.t) cursor rev_segments =
    232 │      let stop = Source_reader.Line.stop line in
    233 │      if
    234 │        (* If [cursor < stop -1], then we require a non-empty end-of-line segment *)
    235 │        Byte_index.(add cursor 1 < stop)
    236 │      then
    237 │        Line.
    238 │          { content = Source_reader.slicei sd cursor stop
    239 │          ; length = Byte_index.(diff stop cursor)
    240 │          ; stag = None
    241 │          }
    242 │        :: rev_segments
    243 │      else rev_segments
    244 │    ;;
    245 │
    246 │    let end_segments ~sd ~(line : Source_reader.Line.t) cursor rev_segments =
    247 │      rev_segments |> add_eol_segment ~sd ~line cursor |> List.rev
    248 │    ;;
    249 │
    250 │    let segments_of_labels ~sd ~(line : Source_reader.Line.t) (labels : Label.t list) =
    251 │      (* 1. Convert labels into a interval set consisting of start and stop points *)
    252 │      let points =
    253 │        let compare_point =
    254 │          (* We lexicographically compare the idx and then we order starting points before stopping points *)
    255 │          Comparable.lift
    256 │            (Comparable.pair
    257 │               Byte_index.compare
    258 │               ((* Compare the tags *) Stdlib.compare : [ `Start | `Stop ] Comparable.t))
    259 │            ~f:(fun (idx, _, start_or_stop) ->
    260 │              let start_or_stop =
    261 │                match start_or_stop with
    262 │                | `Start _ -> `Start
    263 │                | `Stop -> `Stop
    264 │              in
    265 │              idx, start_or_stop)
    266 │        in
    267 │        labels
    268 │        |> List.concat_map ~f:(fun Label.{ range; priority; message } ->
    269 │          let start, stop = Range.split range in
    270 │          [ start, priority, `Start message; stop, priority, `Stop ])
    271 │        |> List.sort ~compare:compare_point
    272 │      in
    273 │      let rev_segments, _, cursor, cursor_labels =
    274 │        (* 2. Iterate through the interval set, maintaining a:
    275 │           + List of (reversed) segments
    276 │           + A priority counter -- a cheap way of deducing the priority of the cursor
    277 │           + A cursor -- the current byte position we're in the interval set
    278 │           + A list of labels starting at the cursor ('cursor labels')
    279 │        *)
    280 │        let eol = Source_reader.Line.stop line in
    281 │        List.fold_left
    282 │          points
    283 │          ~init:([], Priority_count.zero, Source_reader.Line.start line, [])
    284 │          ~f:
    285 │            (fun
    286 │              (rev_segments, priority_count, cursor, cursor_labels)
    287 │              (idx, priority, start_or_stop) ->
    288 │            (* If the next point is at the current cursor and the cursor is before the
    289 │               end of the line ... *)
    290 │            if Byte_index.(cursor = idx) && Byte_index.(idx < eol)
    291 │            then (
    292 │              let priority_count, cursor_msgs =
    293 │                match start_or_stop with
    294 │                | `Start msg ->
    295 │                  (* and the point defined a start of a message, we increment the priority counter
    296 │                     and add the label to the cursor labels. *)
    297 │                  ( Priority_count.start priority_count ~priority
    298 │                  , (priority, msg) :: cursor_labels )
    299 │                | `Stop ->
    300 │                  (* and the point defined a end of a message, we decrement the priority counter. *)
    301 │                  Priority_count.stop priority_count ~priority, cursor_labels
    302 │              in
    303 │              rev_segments, priority_count, cursor, cursor_msgs)
    304 │            else (
    305 │              (* otherwise, we create a segment from 'cursor' to 'idx' and set 'cursor' to 'idx' *)
    306 │              let content = Source_reader.slicei sd cursor idx in
    307 │              let segment =
    308 │                Line.
    309 │                  { content
    310 │                  ; length = Utf8.length content
    311 │                  ; stag =
    312 │                      Priority_count.priority priority_count
    313 │                      |> Option.map (fun priority ->
    314 │                        { priority; inline_labels = cursor_labels })
    315 │                  }
    316 │              in
    317 │              let priority_count, cursor_labels =
    318 │                match start_or_stop with
    319 │                | `Start msg ->
    320 │                  Priority_count.start priority_count ~priority, [ priority, msg ]
    321 │                | `Stop -> Priority_count.stop priority_count ~priority, []
    322 │              in
    323 │              segment :: rev_segments, priority_count, idx, cursor_labels))
    324 │      in
    325 │      assert (List.is_empty cursor_labels);
    326 │      (* 3. Add end-of-line segment to end the line *)
    327 │      let segments = end_segments ~sd ~line cursor rev_segments in
    328 │      segments
    329 │    ;;
    330 │
    331 │    let line_of_labels ~sd ~line labels multi_line_labels =
    332 │      let segments = segments_of_labels ~sd ~line labels in
    333 │      { line = line.idx
    334 │      ; it =
    335 │          Line.
    336 │            { segments
    337 │            ; multi_line_labels
    338 │            ; margin_length = margin_length_of_string (Source_reader.Line.slice ~sd line)
    339 │            }
    340 │      }
    341 │    ;;
    342 │
    343 │    let add_contextual_lines ~sd lines =
    344 │      (* A contextual line is a line satisfying one of:
    345 │         + withing +/-1 lines of a multi-line label (top or bottom) ('multi line label contextual lines')
    346 │         + between two other rendered lines ('gap' contextual lines) *)
    347 │      let add_multi_line_label_contextual_lines =
    348 │        List.concat_map_with_next_and_prev
    349 │          ~f:(fun (l2 : Line.t with_line) ~prev:l1 ~next:l3 ->
    350 │            if List.is_empty l2.it.multi_line_labels
    351 │            then [ l2 ]
    352 │            else (
    353 │              let next_line = Line_index.(add l2.line 1) in
    354 │              let prefix =
    355 │                match l1 with
    356 │                | None when Line_index.(l2.line > initial) ->
    357 │                  (* No preceeding line and [l2] isn't the first line *)
    358 │                  [ line_of_idx ~sd Line_index.(sub l2.line 1) ]
    359 │                | Some l1 when Line_index.(diff l2.line l1.line) > 1 ->
    360 │                  (* Preceeding line is not the immediately preceeding line to [l2] *)
    361 │                  [ line_of_idx ~sd Line_index.(sub l2.line 1) ]
    362 │                | _ -> []
    363 │              in
    364 │              let postfix =
    365 │                match l3 with
    366 │                | None when Line_index.(next_line < (Source_reader.Line.last sd).idx) ->
    367 │                  [ line_of_idx ~sd next_line ]
    368 │                | Some l3 when Line_index.(diff l3.line l2.line) > 1 ->
    369 │                  [ line_of_idx ~sd next_line ]
    370 │                | _ -> []
    371 │              in
    372 │              prefix @ [ l2 ] @ postfix))
    373 │      in
    374 │      let add_gap_contextual_lines =
    375 │        List.concat_map_with_next ~f:(fun l1 ~next:l2 ->
    376 │          match l2 with
    377 │          | None -> [ l1 ]
    378 │          | Some l2 ->
    379 │            let line_delta = Line_index.(diff l2.line l1.line) in
    380 │            if line_delta <= 0
    381 │            then assert false
    382 │            else if line_delta = 1
    383 │            then [ l1 ]
    384 │            else if line_delta = 2
    385 │            then [ l1; line_of_idx ~sd Line_index.(add l1.line 1) ]
    386 │            else (* [line_delta > 2] *)
    387 │              [ l1 ])
    388 │      in
    389 │      lines |> add_multi_line_label_contextual_lines |> add_gap_contextual_lines
    390 │    ;;
    391 │
    392 │    let group =
    393 │      let splitting_threshold = 2 in
    394 │      let block ~start ~rest = { start = start.line; lines = start.it :: rest } in
    395 │      let rec loop = function
    396 │        | [] -> assert false
    397 │        | [ l ] -> l, [], []
    398 │        | l1 :: ls ->
    399 │          let l2, ls, blocks = loop ls in
    400 │          if Line_index.(diff l2.line l1.line) >= splitting_threshold
    401 │          then l1, [], block ~start:l2 ~rest:ls :: blocks
    402 │          else l1, l2.it :: ls, blocks
    403 │      in
    404 │      function
    405 │      | [] -> []
    406 │      | ls ->
    407 │        let l, ls, blocks = loop ls in
    408 │        block ~start:l ~rest:ls :: blocks
    409 │    ;;
    410 │
    411 │    module Line_labels = struct
    412 │      type t =
    413 │        { mutable inline_labels : Label.t list
    414 │        ; mutable multi_line_labels : Multi_line_label.t list
    415 │        }
    416 │
    417 │      let create () = { inline_labels = []; multi_line_labels = [] }
    418 │
    419 │      let add_inline_label t inline_label =
    420 │        t.inline_labels <- inline_label :: t.inline_labels
    421 │      ;;
    422 │
    423 │      let add_multi_line_label t multi_line_label =
    424 │        t.multi_line_labels <- multi_line_label :: t.multi_line_labels
    425 │      ;;
    426 │    end
    427 │
    428 │    let lines_of_labels ~sd labels =
    429 │      let range = range_of_labels labels in
    430 │      let enumerated_labels = List.mapi labels ~f:(fun i label -> i, label) in
    431 │      let _, rev_lines =
    432 │        Iter.fold
    433 │          (Source_reader.lines_in_range sd range)
    434 │          ~init:(enumerated_labels, [])
    435 │          ~f:(fun (labels, rev_lines) (line : Source_reader.Line.t) ->
    436 │            let line_start, line_stop = Source_reader.Line.split line in
    437 │            (* 1. Initialize per-line state *)
    438 │            let line_labels = Line_labels.create () in
    439 │            (* 2. Split labels into [inline_labels] and [multi_line_labels] and filter labels that end at this line *)
    440 │            let labels =
    441 │              List.filter labels ~f:(fun ((id, label) : _ * Label.t) ->
    442 │                let label_start, label_stop = Range.split label.range in
    443 │                if Byte_index.(line_start <= label_start && label_stop <= line_stop)
    444 │                then (
    445 │                  (* Inline label *)
    446 │                  Line_labels.add_inline_label line_labels label;
    447 │                  false)
    448 │                else if Byte_index.(line_start <= label_start && label_start <= line_stop)
    449 │                then (
    450 │                  (* Multi-line label that starts *)
    451 │                  Line_labels.add_multi_line_label line_labels
    452 │                  @@ Multi_line_label.Top
    453 │                       { id
    454 │                       ; start = Column_number.of_byte_index label_start ~line ~sd
    455 │                       ; priority = label.priority
    456 │                       };
    457 │                  true)
    458 │                else if Byte_index.(label_start < line_start && line_stop < label_stop)
    459 │                then (* Multi-line label that goes through this line *)
    460 │                  true
    461 │                else if Byte_index.(label_start < line_start && label_stop <= line_stop)
    462 │                then (
    463 │                  (* Multi-line label that stops through this line *)
    464 │                  Line_labels.add_multi_line_label line_labels
    465 │                  @@ Multi_line_label.Bottom
    466 │                       { id
    467 │                       ; stop = Column_number.of_byte_index label_stop ~line ~sd
    468 │                       ; priority = label.priority
    469 │                       ; label = label.message
    470 │                       };
    471 │                  false)
    472 │                else (* Label starts on a later line *)
    473 │                  true)
    474 │            in
    475 │            (* Add to [rev_lines] *)
    476 │            ( labels
    477 │            , line_of_labels
    478 │                ~sd
    479 │                ~line
    480 │                line_labels.inline_labels
    481 │                line_labels.multi_line_labels
    482 │              :: rev_lines ))
    483 │      in
    484 │      List.rev rev_lines
    485 │    ;;
    486 │
    487 │    let block_of_labels ~sd labels =
    488 │      labels |> lines_of_labels ~sd |> add_contextual_lines ~sd |> group
    489 │    ;;
    490 │
    491 │    let of_diagnostic Diagnostic.{ severity; message; code; labels; notes } =
    492 │      let sources =
    493 │        labels
    494 │        |> group_labels_by_source
    495 │        |> List.map ~f:(fun (source, labels) ->
    496 │          let sd = Source_reader.open_source source in
    497 │          { source
    498 │          ; locus = locus_of_labels ~sd labels
    499 │          ; blocks = block_of_labels ~sd labels
    500 │          })
    501 │      in
    502 │      { severity; message; code; notes; sources = Rich sources }
    503 │    ;;
    504 │  end
    505 │
    506 │  let of_diagnostic = Of_diagnostic.of_diagnostic
    507 │
    508 │  module Compact_of_diagnostic = struct
    509 │    let of_diagnostic Diagnostic.{ severity; message; code; labels; notes } =
    510 │      let sources =
    511 │        labels
    512 │        |> group_labels_by_source
    513 │        |> List.map ~f:(fun (source, labels) ->
    514 │          let sd = Source_reader.open_source source in
    515 │          let locus = locus_of_labels ~sd labels in
        │                      ^^^^^^^^^^^^^^^^^^^^^^^^^^ Bananad from here

    large_files.rs:515:21: error: Some dramatic error
    |}]
;;
