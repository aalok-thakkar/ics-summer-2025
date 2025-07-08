exception NotImplementedError of string
let todo msg = fun _ -> raise (NotImplementedError msg) [@@warning "-27"]

(* Song: Welcome to the Internet *)





(* Lecture 1: Introduction, Logistics, and Course Outline *)

(* 
    Fun Fact: Entomophobia is the fear of insects and affects up to 6% of 
    people. It is believed to be rooted in our ancestors' need to avoid 
    dangerous insects.
    *)



















(* 
    Coding is just a language. You can learn coding anywhere today. YouTube, 
    from your friends, or just using ChatGPT! You don't need a course like 
    this. Learn to code, get a bootcamp certificate. Get a job!














    Most people do that, and I invite you to try. 


    Here is the truth: programming is not just about writing code that works.
    It is about writing code that is correct. 
    
    Reliable. 
    Verified. 
    Understandable.
    Ethical. 
    Robust. 
    
    And when one skips that understanding — bad things happen.





    Case Study I: Knight Capital, 2012

    In 2012, Knight Capital, one of the biggest trading firms in the world,
    deployed new code to handle NYSE's "Retail Liquidity Program" (RLP). But 
    they forgot to deploy the updated version of their high-frequency trading 
    system, SMARS, to all servers.

    Their system used a flag (FID 50) for RLP participation. But old SMARS code
    read FID 50 as "enable PowerPeg" (a legacy order type). PowerPeg was dead 
    code—designed to buy/sell aggressively until filled.

    When the market opened, the system began buying and selling stocks 
    uncontrollably, executing over 4 million trades in 154 stocks. In 45 
    minutes, they lost $440 million.

    1. Knight Capital’s stock price collapsed by over 70% in two days.
    2. The company needed a $400 million emergency bailout.
    3. Knight Capital was acquired by a rival within one year.

    Code correctness isn’t just a technical detail-it’s a business necessity.



    Case Study II: Volkswagen Emissions Scandal, 2015

    Volkswagen programmed their cars’ software to cheat emissions tests. During 
    lab tests, the software made the cars appear clean. On real roads, the cars
    polluted up to 40 times the legal limit.

    Over $30 billion in fines and lawsuits.
    Top executives resigned or were prosecuted.
    VW’s reputation was destroyed.
    Real environmental harm was caused.

    Code correctness isn’t just about “does it work?” It’s about “is it right?”



    
    
    Question: If ChatGPT generated your code, what is the chance that it will 
    mess up? What is the possibility that it will be unfair or unethical? 










    Exercise: Here are three procedures that look correct. Analyse the threats.

*)












(* 
    Welcome to CS-1102 Introduction to Computer Science. 




    A little bit about me:

    Instructor: Aalok Thakkar
    Email: thakkar@ashoka.edu.in
    Office: AC04-705
    Office Hours: Fridays from 10AM to noon





    What is This Course About?

    












    Every undergraduate course is really about knowledge. 
    
    And what is knowledge?

    There are two types of knowledge.














    1. Declarative Knowledge

        Assertions of Truth: 
            A square root of a non-negative real number x is a non-negative
            real number y such that y^2 = x.






        At best you can test if the assertion is true. 



    2. Imperative Knowledge

        How do you compute square roots? 




        Heron of Alexendria (60 CE) offers divide and average.
        Aryabhata (750 CE) in Aryabhatiya offers long division. 














    An instructor is a tour guide. 






    Role of a Student:

    1. Own Your Success. 
    2. Plan First. Stress Less.
    3. Code Daily. Grow Daily.
    4. Fail Fast. Learn Faster.
    5. Integrity First. Always.
    6. Speak Up.
    7. Grow Together.












    Time Commitment: At least 100 hours.


    Objectives:

    1. Learn to precisely specify computational problems and develop solutions 
    that are correct by construction. Prove their correctness using induction, 
    loop invariants, and other formal methods of reasoning.

    2. Develop the ability to analyze and design algorithms as systematic 
    problem-solving approaches, with an emphasis on precision and efficiency.

    3. Build the ability to read, understand, and critically analyze code. Gain
    hands-on experience coding in OCaml, with opportunities to explore other 
    languages such as C, Haskell, Kotlin, Prolog, Python, and Rust.
    
    4. Explore computer science through the functional and the imperative 
    model of computation.

*)

(*
    Logistics:

    Course Webpage: tinyurl.com/ics-summer-2025

    Please join the Google Classroom today.


    TAs: Charchit Agarwal, Vedant Gautum

    TA Office Hours:
        Vedant:
        Charchit:



    How This Course Will Run:

    Students are expected to code on their own.

    Weekly ungraded problem sets

    Weekly assessements on Fridays:
        Week 1: Written Test (math)
        Week 2: Coding Test
        Week 3: Viva
        Week 4: Written Test (analysis)
        Week 5: Coding Test
        Week 6: Final Exam

    Three take-home assignments.




    Grading: 

    Prioritize understanding and growth, rather than focusing on optimizing for
    grades. To encourage this, we will not specify the points but detailed 
    feedback. At a high level, In-Class Assessments, Take-Home Assignments, and
    the Final Exam contribute to equal portions of the grade.


    Policies: 
    
    Plagiarism, collaboration, and use of AI tools are prohibited in in-class
    assessments and final exam. All violations will result in a F grade for the
    course. Please familiarize yourself with the policies and sanctions.

    Any Questions?
*)

(* 

    Today we will see how we can express simple information and manipulate it
    in OCaml.

*)

type day =
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

(* 

    Let us create a function that computes with days. Say for example,
    we want to compute the next day. 
*)


let next_day (d: day) : day = 
    match d with
    | Monday -> Tuesday
    | Tuesday -> Wednesday
    | Wednesday -> Thursday
    | Thursday -> Friday
    | Friday -> Saturday
    | Saturday -> Sunday
    | Sunday -> Monday 

(* 

    Now, we can use this function to compute the next day of any given day.

let next_weekday = todo "Implement next_weekday" *)

let next_weekday (d: day) : day = 
    match d with
    | Monday -> Tuesday
    | Tuesday -> Wednesday
    | Wednesday -> Thursday
    | Thursday -> Friday
    | Friday -> Monday
    | Saturday -> Monday
    | Sunday -> Monday




(* Interactive Mode *)

(*
    Following the same pattern, we could build a boolean type:
*)

type bool =
  | True
  | False

let negb (b : bool) : bool = 
    match b with 
    | True -> False
    | False -> True
;;

let andb (b1 : bool) (b2 : bool) : bool = 
    match b1 with 
    | True -> (
        match b2 with
        | True -> True
        | False -> False )      (* If these brackets would be missing *)
    | False -> False

let andb2 (b1 : bool) (b2 : bool) : bool = 
    match b1 with 
    | True -> b2
    | False -> False

let andb3 (b1 : bool) (b2 : bool) : bool = 
    match (b1, b2) with 
    | (True, True) -> True
    | _ -> False
;;

let orb (b1 : bool) (b2 : bool) : bool = 
    match (b1, b2) with 
    | (False, False) -> False
    | _ -> True


type rgb =
  | Red
  | Green
  | Blue

type color =
  | Black
  | White
  | Primary of rgb

let is_red (c : color) : bool =
    match c with
    | Primary Red -> True
    | _ -> False

(* 

    Type Checking:

    is_red Red

*)

let black_or_white (c : color) : bool = 
    match c with
    | Black -> True
    | White -> True
    | _ -> False

(* Native Types *)

let x : int = 5
let y : int = 2
let sum (x: int) (y: int): int = x + y
let product (x: int) (y: int): int = x * y
let quotient (x: int) (y: int): int = x / y
let remainder (x: int) (y: int): int = x mod y

let day_to_int (d : day) : int =
    match d with
    | Sunday -> 0
    | Monday -> 1
    | Tuesday -> 2
    | Wednesday -> 3
    | Thursday -> 4
    | Friday -> 5
    | Saturday -> 6
    
let int_to_day (n : int) : day =
    match n with
    | 0 -> Sunday
    | 1 -> Monday
    | 2 -> Tuesday
    | 3 -> Wednesday
    | 4 -> Thursday
    | 5 -> Friday
    | 6 -> Saturday
    | _ -> Sunday 

let () = Printf.printf "%d\n" (day_to_int Friday)
    
(* Interpreter Mode *)

let day_to_string (d : day) : string =
    match d with
    | Sunday -> "Sunday"
    | Monday -> "Monday"
    | Tuesday -> "Tuesday"
    | Wednesday -> "Wednesday"
    | Thursday -> "Thursday"
    | Friday -> "Friday"
    | Saturday -> "Saturday"

(* let () = Printf.printf "%s\n" (day_to_string Sunday) *)

let print_day (d : day) : unit =
    Printf.printf "%s\n" (day_to_string d)

let name : string = "Aalok"
let greeting : string = "Hello, " ^ name

let len : int = String.length "hello"

let ch : char = String.get "hello" 1 (* 'e' *)
let ch : char = "hello".[1]

let sub = String.sub "hello" 1 3     (* "ell" *)

let a = "apple" = "apple"            (* true *)
let b = "apple" < "banana"           (* true *)

let s = string_of_int 42             (* "42" *)

(* 
    Compiler Mode

    ocamlc lecture1.ml
    ocamlc -o lec1 lecture1.ml
    
    Compiled Module Interface: Interface information (such as type information 
    and module signatures but no actual code).
    
    Compiled Module Object: Compiled bytecode (intermediate representation) of 
    the code that is executed.

    Executable: Native compiled binary (fast, standalone executable).
    
    ocamlopt lecture1.ml

    Compiled Module eXecutable: Optimized native-code object file

*)