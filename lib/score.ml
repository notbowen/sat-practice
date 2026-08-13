open Model

(* College Board SAT Practice Test 8, fixed-form paper score ranges. *)
let reading_writing_ranges =
  [|
    (200, 200); (210, 220); (210, 220); (210, 220); (210, 220);
    (210, 230); (210, 240); (210, 250); (220, 260); (220, 270);
    (220, 280); (230, 290); (230, 300); (240, 310); (240, 320);
    (250, 330); (270, 330); (300, 340); (310, 350); (320, 360);
    (320, 360); (330, 370); (340, 380); (350, 390); (350, 390);
    (360, 400); (370, 410); (370, 410); (380, 420); (390, 430);
    (400, 440); (410, 450); (410, 450); (420, 460); (430, 470);
    (440, 480); (450, 490); (460, 500); (470, 510); (470, 530);
    (480, 540); (490, 550); (500, 560); (510, 570); (520, 580);
    (530, 590); (540, 600); (560, 620); (570, 630); (580, 640);
    (590, 650); (600, 660); (610, 670); (630, 690); (640, 700);
    (660, 700); (670, 710); (680, 720); (690, 730); (710, 750);
    (720, 760); (730, 770); (750, 770); (760, 780); (770, 790);
    (780, 800); (800, 800);
  |]

let math_ranges =
  [|
    (200, 200); (210, 230); (210, 240); (210, 250); (210, 260);
    (210, 270); (210, 280); (230, 290); (250, 300); (260, 310);
    (280, 340); (300, 360); (320, 360); (330, 370); (340, 380);
    (350, 390); (360, 400); (360, 400); (370, 410); (380, 420);
    (380, 420); (390, 430); (400, 440); (410, 450); (420, 460);
    (430, 470); (430, 470); (440, 480); (450, 490); (460, 500);
    (480, 520); (490, 530); (500, 540); (500, 560); (510, 570);
    (520, 580); (530, 590); (540, 600); (550, 610); (560, 620);
    (580, 640); (590, 650); (600, 660); (610, 670); (630, 690);
    (640, 700); (650, 710); (670, 730); (690, 750); (700, 760);
    (720, 770); (750, 780); (770, 800); (780, 800); (800, 800);
  |]

let round_half_up value = int_of_float (floor (value +. 0.5))

let round_to_ten value = ((value + 5) / 10) * 10

let equivalent_raw ~correct ~total ~curve_max =
  if total <= 0 then 0
  else
    let ratio = float_of_int correct /. float_of_int total in
    max 0 (min curve_max (round_half_up (ratio *. float_of_int curve_max)))

let section_score section ~correct ~total =
  let curve, curve_max =
    match section with
    | Reading_writing -> (reading_writing_ranges, 66)
    | Math -> (math_ranges, 54)
  in
  let raw = equivalent_raw ~correct ~total ~curve_max in
  let low, high = curve.(raw) in
  { low; high; estimate = round_to_ten ((low + high) / 2) }

let total_score rw math =
  let low = rw.low + math.low in
  let high = rw.high + math.high in
  { low; high; estimate = rw.estimate + math.estimate }

let add_leading_decimal_zero answer =
  let length = String.length answer in
  let is_digit_at index =
    index < length && answer.[index] >= '0' && answer.[index] <= '9'
  in
  if length > 1 && answer.[0] = '.' && is_digit_at 1 then
    "0" ^ answer
  else if length > 2 && answer.[0] = '-' && answer.[1] = '.' && is_digit_at 2 then
    "-0" ^ String.sub answer 1 (length - 1)
  else
    answer

let normalize_answer answer =
  answer
  |> String.trim
  |> String.to_seq
  |> Seq.filter (fun c -> c <> ',' && c <> ' ' && c <> '\t' && c <> '\n' && c <> '\r')
  |> String.of_seq
  |> String.lowercase_ascii
  |> Str.global_replace (Str.regexp_string "−") "-"
  |> add_leading_decimal_zero

let is_correct ~submitted ~accepted =
  let submitted = normalize_answer submitted in
  submitted <> ""
  && List.exists
       (fun expected -> String.equal submitted (normalize_answer expected))
       accepted
