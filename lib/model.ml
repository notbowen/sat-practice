type section = Reading_writing | Math

type difficulty = Easy | Medium | Hard

type module_kind = Reading_writing_1 | Reading_writing_2 | Math_1 | Math_2

type item_type = Multiple_choice | Student_response

type progress_status = Done | Incorrect | Skipped

type score_range = {
  low : int;
  high : int;
  estimate : int;
}

type question_metadata = {
  external_id : string;
  question_id : string;
  section : section;
  difficulty : difficulty;
  domain_code : string;
  domain_name : string;
  skill_code : string;
  skill_name : string;
  item_type : item_type option;
  source_updated_at : int64;
  synced_at : int64;
}

type answer_option = {
  letter : string;
  content : string;
}

type question_detail = {
  external_id : string;
  stimulus : string;
  stem : string;
  rationale : string;
  item_type : item_type;
  answer_options : answer_option list;
  correct_answers : string list;
}

type module_assignment = {
  kind : module_kind;
  questions : question_metadata list;
  relaxed_blueprint : bool;
}

let section_to_string = function
  | Reading_writing -> "rw"
  | Math -> "math"

let section_of_string = function
  | "rw" -> Some Reading_writing
  | "math" -> Some Math
  | _ -> None

let section_label = function
  | Reading_writing -> "Reading & Writing"
  | Math -> "Math"

let difficulty_to_string = function
  | Easy -> "E"
  | Medium -> "M"
  | Hard -> "H"

let difficulty_of_string = function
  | "E" -> Some Easy
  | "M" -> Some Medium
  | "H" -> Some Hard
  | _ -> None

let difficulty_label = function
  | Easy -> "Easy"
  | Medium -> "Medium"
  | Hard -> "Hard"

let module_to_string = function
  | Reading_writing_1 -> "rw1"
  | Reading_writing_2 -> "rw2"
  | Math_1 -> "math1"
  | Math_2 -> "math2"

let module_of_string = function
  | "rw1" -> Some Reading_writing_1
  | "rw2" -> Some Reading_writing_2
  | "math1" -> Some Math_1
  | "math2" -> Some Math_2
  | _ -> None

let module_label = function
  | Reading_writing_1 -> "Reading & Writing · Module 1"
  | Reading_writing_2 -> "Reading & Writing · Module 2"
  | Math_1 -> "Math · Module 1"
  | Math_2 -> "Math · Module 2"

let module_short_label = function
  | Reading_writing_1 -> "English 1"
  | Reading_writing_2 -> "English 2"
  | Math_1 -> "Math 1"
  | Math_2 -> "Math 2"

let module_section = function
  | Reading_writing_1 | Reading_writing_2 -> Reading_writing
  | Math_1 | Math_2 -> Math

let module_number = function
  | Reading_writing_1 | Math_1 -> 1
  | Reading_writing_2 | Math_2 -> 2

let module_duration_seconds = function
  | Reading_writing_1 | Reading_writing_2 -> 32 * 60
  | Math_1 | Math_2 -> 35 * 60

let module_question_count = function
  | Reading_writing_1 | Reading_writing_2 -> 27
  | Math_1 | Math_2 -> 22

let module_order = function
  | Reading_writing_1 -> 1
  | Reading_writing_2 -> 2
  | Math_1 -> 3
  | Math_2 -> 4

let item_type_to_string = function
  | Multiple_choice -> "mcq"
  | Student_response -> "spr"

let item_type_of_string = function
  | "mcq" -> Some Multiple_choice
  | "spr" -> Some Student_response
  | _ -> None

let progress_status_to_string = function
  | Done -> "done"
  | Incorrect -> "incorrect"
  | Skipped -> "skipped"

let compare_module a b = Int.compare (module_order a) (module_order b)

let all_modules =
  [ Reading_writing_1; Reading_writing_2; Math_1; Math_2 ]
