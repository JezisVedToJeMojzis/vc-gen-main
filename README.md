# VC Generation

In this assignment, you will implement a verifier based on the weakest-
precondition/VCGen methodology as discussed in class. We will work with programs
that are written in a JavaScript like syntax (EcmaScript) and translate them
into our imperative language Nano.

## Install Z3

This assignment will use Z3 to solve logical formulas. In order to use Z3
from code, you will need to install the developer version of Z3 (not just the
executable). Since we're using [Haskell's Z3 bindings](https://hackage.haskell.org/package/z3),
we need to use a specific version of Z3, namely `4.8.x`. Any other version will
give you compilation errors.

### Package Manager (Linux)

Use your package manager to install the z3 developer version. Here's an example
for Ubuntu.

```sh
$ sudo apt install libz3-dev
```

Make sure that you install the correct version!

### Manually

Go to the [Z3 releases](https://github.com/Z3Prover/z3/releases/tag/z3-4.8.17)
page and pick the release for your OS and architecture.

#### Binary

From this release, copy the `z3` binary into your path.
 - `/usr/local/bin`, for Linux, MacOS.
 - `\Windows\System32\bin`, for Windows.

Make sure you can now run the binary from any other directory. You may need
to open a new terminal for this to work. If this gives you a version number,
then proceed!

```sh
$ z3 --version
```

#### Headers

Move the include files (directory `include`) into a directory for include
files. For example `/usr/local/include`, for Linux and MacOS. You can pick any
directory for Windows. Then, modify the `stack.yaml` file in this directory and
add the path to `extra-include-dirs`. For example:

```yaml
extra-include-dirs: [/usr/local/include]
```

#### Library

Copy the library files (all remaining files in the `bin` directory) and copy
them to your library path. For example `/usr/local/lib` for Linux, MacOS, or any
directory for Windows. Again, you need to update your `stack.yaml` to add the
library path:

```yaml
extra-lib-dirs: [/usr/local/lib]
```

## Running and testing

This code again features a test bench, which you may run in the same fashion
with `stack`. The tests again aim to direct you through to code base of this
assignment and we strongly suggest you follow this!

This assignment also features an executable which may be ran with the following
command.

```
$ stack run -- -f <path-to-file>
```

This will run the verifier on a single file. Again, you are allowed to make
modification to `Main` if you wish to get more debug information.

## Assignment

The assignment consists of two main parts. First you will write a weakest-
precondition verifier. After this, you will use this verifier to prove
properties about a set of small programs.

### Nano DSL

To get a grasp of what Nano DSL looks like, you can look at any of the programs
in the `programs` folder. One of the main things to note is that we interpret
some function calls as Nano statements.

Statement `assume(F)` encodes an assumption that formula F holds. 
Statement `assert(F)` encodes an obligation to show that formula F holds. 
In particular a Hoare triple {P} s {Q} can be translated into the following
expression.

```js
assume(P); s; assert(Q)
```

Statements `invariant(P)` places the given invariant `P` as (part of) the
invariant of the closest scoping while.

Statements `requires(P)` and `ensures(P)` respecitvely set a pre or post 
condition on the function signature.

Statements `modifies(x)` says that the inner variable will be modified by
the function call.

Expressions `forall(x, P)` and `exists(x, P)` quantify `x` over `P` and may
be used wherever logic is involved (i.e. all of these prior mentioned calls).

**Aside:** Embedding a new language (Nano) into an existing language
(Haskell) is a common and important [technique](http://wiki.haskell.org/Embedded_domain_specific_language)
in programming language research. The embedded language is often called a Domain
Specific Language (DSL). We're using an approached called `deep embedding`.

### Verifier

To get a working verifier, you will have to implement roughly the following
steps.

1. Substition of logical formulas
2. Conversion of ECMAScript into Nano
3. Generation of Verification Conditions from a Nano program

Regarding the ECMAScript conversion, you may find the documentation of the
parser [here](https://hackage.haskell.org/package/language-ecmascript-0.17.0.1/docs/Language-ECMAScript3-Syntax.html).
Make sure to check out the nano programs to form a better understanding of what
the embedding looks like.

For your verifier, it is just as important that your code rejects bad programs
as it is to proof good programs. As such, we feature a bunch of programs to
achieve both goals. Programs in the `programs/pos` directory should pass
verification, while programs in the `programs/neg` directory should fail
verification.

### Verification

You will have to verify all the tests in `programs/verify`. As you are probably
already aware of at this point, we need to provide invariants to prove properties
when loops are involved.

You may add invariants to the files in this folder in order to verify them.
We do check that you do not modify the code in any other way.

**Tip** Instead of writing 

```js
invariant(P && Q && R)
```

you can write 

```js
invariant(P);
invariant(Q);
invariant(R);
```

# Extension for pointers (Final project)
To run the code you can use stack test.
## Parse (statement)

### To simulate pointers in Javascript we decided to use:

- JS.OpAssignSpRShift - for referencing (>>=) (LoadPtr)


- JS.OpAssignLShift - for dereferencing (<<=) (StorePtr)

### Tested JS code: 
Pointer ptr points to variable x. If 5 is assigned to ptr then it is assigned to x as well.
```
function pointer() {
    x >>= ptr; // mapping x to pointer ptr (LoadPtr)
    
    ptr[x] <<= 5; // storing value 5 into ptr (StorePtr)

    assert(x==5); // x should be 5
}

```

### Test script: 
```
check "programs/pos/pointer.js" 
      [ Function
        { fname = "pointer"
        , fargs = []
        , fpre = true
        , fpost = true
        , fmods = []
        , fbody = Seq
          [ 
            LoadPtr "x" "ptr"
          , StorePtr "ptr" (Var "x") (Const 5)
          , Assert . Pred $ (Var "x") :==: (Const 5)
          ]
        }
      ]
}

```

## Nano (vcgen)
In Nano.hs we created a global array called *memory* where we collect needed information. Our variable *x* in js code is then substituted for this array.
### Load

After Loading pointer we substitute our global array for:


**Store (Array "$memory") (Var "ptr") (Var "x")**

Which is equal to:  

**$memory[ptr] = x**

Where $memory is name of our global array, ptr is name of the pointer and x is variable to which is the pointer pointing.

### Store 
Here we check our global array for index of the pointer.
If the index is equal to pointer, we substitute the value (x) on that index and we get: 


**Store (Store (Array "$memory") (Var "ptr") (Var "x")) (Var "ptr") (Const 5)**

Which is equal to:  

**$memory[ptr] = 5**

After substituting the variable x for array memory, we can see that the value 5 was mapped to x. This can be observed in the generated VC's of programs/neg/pointer.js

**Note:** Our code contains quite a lot of **commented out code in the Nano.hs file**, we thought we would leave it in to illustrate the different approaches we tried (that did not end up working). Feel free to skip over those if needed. 

The final code doesn't pass the positive test due to the issues decribed in the Readme and the project report, however **the correct functionality of pointers is illustrated by observing the VC's generated by "programs/neg/pointer.js"**.
