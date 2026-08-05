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
  ]

let dangerous_tags =
  [
    "script"; "style"; "iframe"; "object"; "embed"; "form"; "input";
    "button"; "textarea"; "select"; "option"; "foreignobject"; "use";
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
    "separator"; "columnalign"; "rowalign"; "columnspan"; "rowspan"; "notation";
  ]

let svg_attributes =
  [
    "xmlns"; "viewbox"; "width"; "height"; "x"; "y"; "x1"; "x2"; "y1";
    "y2"; "cx"; "cy"; "r"; "rx"; "ry"; "d"; "points"; "fill"; "stroke";
    "stroke-width"; "stroke-linecap"; "stroke-linejoin"; "stroke-dasharray";
    "opacity"; "fill-opacity"; "stroke-opacity"; "transform"; "font-family";
    "font-size"; "font-weight"; "text-anchor"; "dominant-baseline"; "id";
    "patternunits"; "patterntransform"; "data-name";
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
         | [ property; setting ]
           when String.equal (String.lowercase_ascii (String.trim property)) "text-align" ->
             let setting = String.lowercase_ascii (String.trim setting) in
             if List.mem setting [ "left"; "right"; "center" ] then
               Some ("text-align: " ^ setting)
             else None
         | _ -> None)
  |> String.concat "; "

let allowed_attribute tag name value =
  let name = String.lowercase_ascii name in
  if String.starts_with ~prefix:"on" name then None
  else if String.equal name "class" then
    let classes = safe_class value in
    if String.equal classes "" then None else Some classes
  else if String.equal name "style" && is_member tag html_tags then
    let style = sanitize_style value in
    if String.equal style "" then None else Some style
  else if List.mem name global_attributes then Some value
  else if is_member tag mathml_tags && List.mem name mathml_attributes then Some value
  else if is_member tag svg_tags && List.mem name svg_attributes then Some value
  else if List.mem name (html_attributes tag) then
    if List.mem name [ "src" ] then
      if safe_media_url value then Some value else None
    else Some value
  else None

let sanitize_element element =
  let tag = name element in
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
