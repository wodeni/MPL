# PLTProject
Implementing a new programming language.


--------

## PLT Meeting Chy and Nimo 04/16

- Note that when we use `printm`, ``llc` with default settings, and finally use `gcc` to link them all, there will be an error complaining about something really weird. We managed to fix that using `-relocation-model=pic`

## PLT Meeting David, Chy, and Nimo 04/07

- Tasks and tentative deadlines:
    - 04/13/2017: __definitions of entry functions__
        - What are the parameters
        - Scope of variables
    - 04/13/2017: `Img`
    - 04/24/2017: __`fMat`__
    - 04/27/2017: apply operator
    - 04/27/2017: `imgread` and `imgwrite`
    - 05/01/2017: Game of life
- Meetings
    - Monday   : 20:00 ~ 22:00
    - Tuesday  : 15:00 ~ 16:30
    - Thursday : 20:00 ~ 22:00
    - Sunday   : 18:00 ~ 21:00

# Useful links

- The CLAM project report(2011): [Link](http://www.cs.columbia.edu/~sedwards/classes/2011/w4115-fall/reports/CLAM.pdf)

# Meeting notes

## PLT Meeting with Julie 03/08

- Report looks okay, not much to say
- Hello world program should not be very complicated. It can simply make a matrix, do some operations on it, and then print it out.
    - NOTE: we can leave the semantic checker at the end of the hello world development if we want, because it is optional for correct code to be executed.
- Walk through of the program:
    - Scanner -> Parser -> AST -> (semantic checker) -> codegen -> executable
    - The entire workflow is linked by a driver file. (In MicroC this is called `microc.ml`)
- Look at [llvm.moe](https://llvm.moe/) for documentation for llvm code generation
    - include `Ast` and `llvm` as ocaml modules
    - There is a concept in llvm also called "module". Normally the entire language is in the same llvm module
- Test routine walk through
    - test files include two parts
        - (1) the "golden references"
        - (2) the source files: `.mpl` files
    - The "golden references" are written by us, which is the expected output in stdin/stderr
    - The program will compare the actual output of the test to the "golden reference".
- Function matrix: looks like it is possible to do so with function pointers. We need to make sure there is such thing in llvm moe.
- Plan for Spring Break: We need to complete the hello world before the end of the break. The work is split into the following
    - Scanner
    - Parser
    - Ast
    - Code generation
    - Test causes
    - Compilation/makefile/other software engineering issues
- We will discuss the allocation of work remotely    
- Enjoy the break guys!

## PLT Group Meeting 02/26

- Deliverables coming up
    - Hello world: March 27th
- CFG: Need to finish
    - Start implementing:
- Micro-c: a mess on MAC OS, working on linux
- Timeline:
    - March 8th: 4115 Exam / Meeting - Have the first git commit before this
    - March 11th: Spring break starts
    - During spring break: get the hello world working
- Jane will email Julie about the translation to C: What makes our language so high-level??

## PLT Group Meeting 02/16

- With TA: Julie Chen
- Case for matrix of functions: image -> edge detection -> edges -> coloring function -> color all edges red
- LRM: include a better working sample for matrix of functions.
- What about kernels with larger size? Should we increase the scope of access??
- Should we compile the language all the way to LLVM? Or C?
- Mark the type of the parameter in the function signature
- Function as first class object... Is this possible to do so in LLVM??
- Look at Julie's LRM as reference: TBag - 3 semester ago
- Suggestion: Look ahead. Get the Hello World done before **the beginning of March**!!
- Look at Micro-C  as reference.
- When generating LLVM code, use Moe
- Check with Juile every week or so

## PLT Group Meeting 02/05

- End goal of the project: a demo of game of life - we have to have functions that can look at the neighbors and make decisions! The end product will be a program that produces hundreds of frames and animate it(no necessarily using our language though).
- Do we allow composite functions?
    - Chy thought about two functions adding together, but after discussion we realize the type of that addition will be problematic. So even if we do allow operations among functions, it will just be composition now.
- Do we allow global access to the entire matrix for functions?
    - For now, no. We run into the dimensionality problem again. Should we check this in Compile time or run time? Is it even possible to check?
- Syntax of the function: how do we refer to neighbors inside of the function?
    - The "#" keywords:

| #NW | #N | #NE |
| --- |
| #W | #C | #E |
| #SW | S | #SE|

- Edge of the matrix: When ever we try to access one element outside of the matrix border, we paddle it with zero, just to make life easier. This will make the game of life example possible.

- Starting on the drafting of proposal: [Link](https://www.overleaf.com/8021175tsmvpcnwryqs#/28283903/)
    - IntrO: Jane
    - Language feature: Nimo
    - Syntax: David
    - Sample code: Chy
- Next meeting: depends on the quality of the proposal draft, we will be exchanging revision ideas over messenger

## PLT Group Meeting 02/03

- Little debate on Poly vs. Matrix

| Poly | Matrix |
| --- |
| Differentiation | Matrix calculations |
|  | Image processing |
|  | Single-variable polynomial representation |

- Decided on doing the matrix language. since it is easier in terms of math
- The novel feature of the language will be: matrix of functions. This can be the same thing as the "Embedded Cstring" feature in CLAM. We process every single pixel of the image by applying a matrix of functions on it
- Syntax:

```
struct Img {
    Mat R, G, B;
}

Img image = imgread("test.png")

Img.R =

func f1(int t) { t = t + 1 }
func f5(double t) { t = t + 1 }
func f2() { }
func f3() { }
func f4() { }

Mat A = { 1 2;
        3 4 }
Mat C = { 1 2;
        3 4 }
Mat F = { f1 f2;
          f3 f4;
          f3 f4 }

Mat B = A * F @ C

FMat<int> F2 = { f1 f1;
                 f1 f5 }

Mat D = F2 @ A
Mat D = f1 @@ A


// Note that this is equal to "A * (F @ C)"
// Compilation error if dimensions of the two operands of @ don't match
```

- Structure of the report
    - Introduction: paraphrasing CLAM
    - Lexical Convention:
    - Feature
    - Example program
- Objective of the next meeting: finish the proposal
    - Name of the project!
- Distribution of tasks:
    - Nimo: intro, look into convolution/kernel stuffs
    - David: Latex document setup, language syntax
    - Chy: syntax, possible new features. Sample program
    - Jane is the boss.


## PLT Group Meeting 02/01

- Discussion on the main theme of the project
    - Conclusion: **MatLab but worse** (Lie: light-weight computer algebraic system)
- Possible features of the language:
    - Native support for polynomials; MATLAB represents this using a row vector. Chy suggested we could represent multi-variable polynomial using a product of matrices.
    - Matrix: similar to `Mx`, we can have matrix built in and basic manipulations of them. Possible applications include convolution operator `**`. Kernel and image defined using Matrix type.
    - Manipulations on polynomials
        - Find root: Newton's method
        - Find derivatives: should be simple
        - Graphing them: see `Mx`, but LLVM could be tricky to deal with.
        - Polynomial interpolation: David
    - Possible implementation for `struct`: as David mentioned, our representation of polynomials can be really inefficient if the order is too high. A struct will be a solution to this.
- Timeline:
    - Proposal due: Feb. 8th
    - Two meetings before submission
        - meeting 1(Tomorrow): agree on the syntax
        - meeting 2(regular): assemble the report
- Objectives and preview for next meeting
    - Need to agree on the syntax of the language.
    - Symbolic difference between matrices and numerical variables
    - A matrix of functions? (Chy)


## PLT Group Meeting 01/29

### 1. Idea brainstorm

- Turtle(David): drawing fractals
    - "fork and exec" turtles based on certain condition
    - Can be used to produce 2D fractal images and (possibly) 3D ones.
    - So far, we are all interested in doing this one.
- Chemistry Language(Chy)
    - Looks good at the beginning. But when we discussed further about the possible extension to ChemLAB(previous project). We realized that external libraries are going to be needed for the empirical data in chemistry, such as the atomic mass for Cs.
    - Also discussed the possibility of drawing the molecules. This turns out to be fairly complicated because there are often many ways to draw a compound or molecule.

### 2. Next meeting plan

- Objective: Begin the drafting of proposal and distribute the work; report on out research on the theme and refine the topic of the project.
- Distribution of work:
    - Jane: read the previous project in detail (http://www.cs.columbia.edu/~sedwards/classes/2015/4115-fall/reports/frac.pdf)
    - David: investigation on the advantages of having nondeterminism for the language and the feasibility of this feature
    - Nimo: investigation on the feasibility of drawing 3D fractals using OpenGL
    - Chy: discover possible new features

## PLT Group Meeting 01/25

### 1. Group Administration

- Group norms:
    1. no interruption when one group member is speaking;
    2. all decision should be made unanimously;
    3. when running late, please let the group know in advance;
    4. pre-meeting proposals should be made by each group member prior to meeting;
    5. always plan for the next meeting at the end of each meeting.
- Note taker during meetings: Nimo
- Group GitHub host: David

### 2. DeadLines

| Deliverables | DeadLine |
| --- | --- |
| Proposals | Feb. 8th |
| Language Manual | Feb. 22nd |
| Hello World | Mar. 27th |
| Report | May. 10th |

- NOTE: it will be wise to finish everything before May 1st due to the finals week

### 3. Ideas for the project

- David found the website containing design ideas:
    - https://esolangs.org/wiki/List_of_ideas
- Chemistry(Chy): use primitives to represent atoms(or even protons etc.), build functions to represent reactions. Loops/Recursion can be used to model chain reactions
- Biology(Chy): Use some data structure to represent creatures and can possibly model evolution.
    - DNA language can also be an idea.
- Geometry language: use basic types to represent lines etc. It is possible to prove geometry theorems(?)

### 4. Next meeting plan

- Time: next regular meeting time
- Objective: To decide on the idea to be implemented
    - A short discussion on writing the proposal will follow
