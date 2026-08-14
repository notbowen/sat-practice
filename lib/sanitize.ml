open Soup

let html_tags =
  [
    "p"; "span"; "div"; "br"; "strong"; "b"; "em"; "i"; "u"; "sup";
    "sub"; "ul"; "ol"; "li"; "table"; "thead"; "tbody"; "tfoot"; "tr";
    "th"; "td"; "caption"; "figure"; "figcaption"; "blockquote"; "hr";
    "img"; "audio"; "source";
  ]

let mathml_tags =
  [
    "math"; "mrow"; "mn"; "mi"; "mo"; "mfrac"; "mfenced"; "msup";
    "msub"; "msubsup"; "msqrt"; "mroot"; "mtext"; "mtable"; "mtr"; "mtd";
    "mover"; "munder"; "munderover"; "menclose"; "mspace"; "mpadded";
    "mphantom"; "mstyle"; "semantics"; "annotation";
  ]

let svg_tags =
  [
    "svg"; "g"; "defs"; "pattern"; "rect"; "line"; "text"; "tspan";
    "path"; "circle"; "ellipse"; "polyline"; "polygon"; "title"; "desc";
    "clippath"; "use";
  ]

let dangerous_tags =
  [
    "script"; "style"; "iframe"; "object"; "embed"; "form"; "input";
    "button"; "textarea"; "select"; "option"; "foreignobject";
    "video"; "canvas"; "link"; "meta"; "base";
  ]

let is_member value values = List.exists (String.equal value) values

let safe_class value =
  value
  |> String.split_on_char ' '
  |> List.filter (fun class_name ->
         List.mem class_name [ "sr-only"; "image"; "question-graphic" ])
  |> String.concat " "

let safe_media_url value =
  let lower = String.lowercase_ascii (String.trim value) in
  String.starts_with ~prefix:"data:image/png;base64," lower
  || String.starts_with ~prefix:"data:image/jpeg;base64," lower
  || String.starts_with ~prefix:"data:image/webp;base64," lower
  ||
  match Uri.of_string value |> Uri.host with
  | Some host ->
      let host = String.lowercase_ascii host in
      String.equal host "collegeboard.org"
      || String.ends_with ~suffix:".collegeboard.org" host
  | None -> false

let global_attributes = [ "aria-label"; "aria-hidden"; "role"; "alt"; "alttext" ]

let mathml_attributes =
  [
    "xmlns"; "display"; "mathvariant"; "mathsize"; "stretchy"; "fence";
    "separator"; "open"; "close"; "separators"; "columnalign"; "rowalign";
    "columnspan"; "rowspan"; "notation";
  ]

let svg_attributes =
  [
    "xmlns"; "viewbox"; "width"; "height"; "x"; "y"; "x1"; "x2"; "y1";
    "y2"; "cx"; "cy"; "r"; "rx"; "ry"; "d"; "points"; "fill"; "stroke";
    "stroke-width"; "stroke-linecap"; "stroke-linejoin"; "stroke-dasharray";
    "opacity"; "fill-opacity"; "stroke-opacity"; "transform"; "font-family";
    "font-size"; "font-weight"; "text-anchor"; "dominant-baseline"; "id";
    "patternunits"; "patterntransform"; "data-name"; "clip-path";
  ]

let html_attributes tag =
  match tag with
  | "td" | "th" -> [ "colspan"; "rowspan"; "scope" ]
  | "ol" -> [ "start" ]
  | "img" -> [ "src"; "width"; "height" ]
  | "audio" -> [ "src"; "controls" ]
  | "source" -> [ "src"; "type" ]
  | _ -> []

let sanitize_style value =
  value
  |> String.split_on_char ';'
  |> List.filter_map (fun declaration ->
         match String.split_on_char ':' declaration with
         | [ property; setting ] ->
             let property = String.lowercase_ascii (String.trim property) in
             let setting = String.lowercase_ascii (String.trim setting) in
             if String.equal property "text-align"
                && List.mem setting [ "left"; "right"; "center" ]
             then Some ("text-align: " ^ setting)
             else if
               String.equal property "text-decoration"
               && String.equal setting "underline"
             then
               Some "text-decoration: underline"
             else None
         | _ -> None)
  |> String.concat "; "

(* College Board's matplotlib-generated SVGs carry all of their styling in
   [style] attributes, so stripping them turns every figure into a solid black
   box (the SVG default fill). Keep only the presentation properties those
   figures use, with strictly validated values. [setting] must already be
   trimmed and lowercased. *)
let is_hex_color setting =
  let length = String.length setting in
  List.mem length [ 4; 5; 7; 9 ]
  && setting.[0] = '#'
  && String.for_all
       (fun c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'))
       (String.sub setting 1 (length - 1))

let is_paint_value setting =
  String.equal setting "none"
  || String.equal setting "currentcolor"
  || is_hex_color setting
  || (String.length setting > 0
      && String.for_all (fun c -> c >= 'a' && c <= 'z') setting)
  || (String.starts_with ~prefix:"rgb" setting
      && String.for_all
           (fun c -> (c >= '0' && c <= '9') || List.mem c [ '('; ')'; ','; '.'; '%'; ' ' ])
           setting)

let is_numeric_list setting =
  String.length setting > 0
  && String.for_all
       (fun c -> (c >= '0' && c <= '9') || List.mem c [ '.'; ','; ' '; '-' ])
       setting

let is_clip_path setting =
  (* Only internal fragment references such as [url(#clip1)]. *)
  let length = String.length setting in
  length > 6
  && String.starts_with ~prefix:"url(#" setting
  && setting.[length - 1] = ')'
  && String.for_all
       (fun c -> (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || List.mem c [ '_'; '-'; '.' ])
       (String.sub setting 5 (length - 6))

let is_fragment_ref value =
  (* Same-document references such as [#DejaVuSans-30] only: [<use>] can also
     point at external SVG files, which must stay blocked. *)
  let length = String.length value in
  length > 1
  && value.[0] = '#'
  && String.for_all
       (fun c ->
         (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
         || List.mem c [ '_'; '-'; '.' ])
       (String.sub value 1 (length - 1))

let is_font_value setting =
  (* No parentheses, semicolons or colons, so nothing like [url(...)] or
     declaration smuggling can survive. *)
  String.length setting > 0
  && String.for_all
       (fun c ->
         (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')
         || List.mem c [ ' '; '.'; ','; '-'; '\'' ])
       setting

let svg_style_property_safe property setting =
  match property with
  | "fill" | "stroke" -> is_paint_value setting
  | "stroke-width" | "stroke-dasharray" | "opacity" | "fill-opacity" | "stroke-opacity" ->
      is_numeric_list setting
  | "stroke-linecap" -> List.mem setting [ "butt"; "round"; "square" ]
  | "stroke-linejoin" -> List.mem setting [ "miter"; "round"; "bevel" ]
  | "text-anchor" -> List.mem setting [ "start"; "middle"; "end" ]
  | "clip-path" -> is_clip_path setting
  | "font" | "font-family" | "font-size" | "font-style" | "font-weight" ->
      is_font_value setting
  | _ -> false

let sanitize_svg_style value =
  value
  |> String.split_on_char ';'
  |> List.filter_map (fun declaration ->
         match String.index_opt declaration ':' with
         | None -> None
         | Some index ->
             let property =
               String.sub declaration 0 index
               |> String.trim |> String.lowercase_ascii
             in
             let setting =
               String.sub declaration (index + 1) (String.length declaration - index - 1)
               |> String.trim |> String.lowercase_ascii
             in
             if svg_style_property_safe property setting then
               Some (property ^ ": " ^ setting)
             else None)
  |> String.concat "; "

let allowed_attribute tag name value =
  let name = String.lowercase_ascii name in
  if String.starts_with ~prefix:"on" name then None
  else if String.equal name "class" then
    let classes = safe_class value in
    if String.equal classes "" then None else Some classes
  else if String.equal name "style" && (is_member tag html_tags || is_member tag svg_tags) then
    let style =
      if is_member tag svg_tags then sanitize_svg_style value else sanitize_style value
    in
    if String.equal style "" then None else Some style
  else if is_member tag svg_tags && List.mem name [ "href"; "xlink:href" ] then
    (* matplotlib stamps glyph paths onto figures with [<use xlink:href="#…">];
       only same-document fragments may be referenced. *)
    if is_fragment_ref value then Some value else None
  else if List.mem name global_attributes then Some value
  else if is_member tag mathml_tags && List.mem name mathml_attributes then Some value
  else if is_member tag svg_tags && List.mem name svg_attributes then Some value
  else if List.mem name (html_attributes tag) then
    if List.mem name [ "src" ] then
      if safe_media_url value then Some value else None
    else Some value
  else None

let sanitize_element element =
  (* Foreign-content tag names may arrive camelCased (e.g. clipPath); the
     whitelists are all lowercase, and browsers re-adjust the casing when they
     parse the serialized HTML. *)
  let tag = String.lowercase_ascii (name element) in
  if is_member tag dangerous_tags then delete element
  else if not (is_member tag html_tags || is_member tag mathml_tags || is_member tag svg_tags) then
    unwrap element
  else
    let attributes =
      fold_attributes (fun values name value -> (name, value) :: values) [] element
    in
    List.iter (fun (name, _) -> delete_attribute name element) attributes;
    List.iter
      (fun (name, value) ->
        match allowed_attribute tag name value with
        | Some safe_value -> set_attribute (String.lowercase_ascii name) safe_value element
        | None -> ())
      attributes

let fragment value =
  if String.equal (String.trim value) "" then ""
  else
    let document = parse value in
    document |> descendants |> elements |> to_list |> List.rev
    |> List.iter sanitize_element;
    to_string document
