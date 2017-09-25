# Class Block
## Concept
A block is an [anonymous function](http://en.wikipedia.org/wiki/Anonymous_function) with full closure semantics. In C# terminology, it's a [Delegate](http://en.wikipedia.org/wiki/Delegate_(CLI)). A block is a first-class object: It can be stored as the value of a variable, passed as an argument to a method, or sent messages.

A block can have any number of parameters from zero to 32. As is the case with all variables and parameters in Essence#, block parameters are dynamically typed (they behave as though they had the type _[Dynamic](http://msdn.microsoft.com/en-us/library/dd264741.aspx)_ in C#.)

As is the case for almost all operations on values in Essence#, the way to invoke a block as a function is to send it a message. Here are some examples:

{{
"A zero-argument block which adds 3 and 4. Sending the block the message #value results in the value 7."

[3 + 4](3-+-4) value.}}

{{"A one-argument block (which also uses a one-argument block in its code). The block creates an array whose elements are computed by multiplying the elements of the array #(3 5 8) by a factor. The factor is the block's single parameter. The example evaluates to the array #(6 10 16)."

[:factor ](-#(3-5-8)-collect_-[_each-_-each-_-factor)] value: 2.}}

{{
"A two-argument block which computes a new array by multiplying each element of an array by a factor. The factor is the block's first argument, and the source array is the second argument. This is a more reusable version of the previous example. Sending the message #value: 3 value: #(-1 2 10) results in the array #(-3 6 30)."

[:factor :array ](-array-collect_-[_each-_-each-_-factor)] 
        value: 3 value: #(-1 2 10)}}

## Configuration
superclass: [CompiledCode](CompiledCode);
instanceArchitecture: #Block

_Page still under construction; sorry about that._

## Instance Methods
{{
protocol: #evaluating method: 
[## valueWithExit

        "Evaluate the receiver with a single argument which it can use to exit before evaluating its final statement."

        ^self value: [^nil](^nil)
];

protocol: #'system primitives' method:
[## lexicalContext

        <primitive: lexicalContext domain: Block>
];

protocol: #'system primitives' method:
[## whileNil

        <primitive: whileNil domain: Block>
];

protocol: #'system primitives' method:
[## whileNotNil

        <primitive: whileNotNil domain: Block>
];

protocol: #'system primitives' method:
[## whileNil: nilAction

        <primitive: #whileNil: domain: Block>
];

protocol: #'system primitives' method:
[## whileNotNil: notNilAction

        <primitive: #whileNotNil: domain: Block>
];

protocol: #'system primitives' method:
[## whileFalse

        <primitive: whileFalse domain: Block>
];

protocol: #'system primitives' method:
[## whileTrue

        <primitive: whileTrue domain: Block>
];

protocol: #'system primitives' method:
[## whileFalse: actionBlockObject

<primitive: #whileFalse: domain: Block>
];

protocol: #'system primitives' method:
[## whileTrue: actionBlockObject

        <primitive: #whileTrue: domain: Block>
];

protocol: #'system primitives' method:
[## valueWithArguments: arguments

        <primitive: #valueWithArguments: domain: Block>
];

protocol: #'system primitives' method:
[## value

        <primitive: value domain: Block>
];

protocol: #'system primitives' method:
[## value: a1

        <primitive: #value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2

        <primitive: #value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3

        <primitive: #value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4

        <primitive: #value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5

        <primitive: #value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6

        <primitive: #value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7

        <primitive: #value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8

        <primitive: #value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9

        <primitive: #value:value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9 value: a10

        <primitive: #value:value:value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9 value: a10 value: a11

        <primitive: #value:value:value:value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9 value: a10 value: a11 value: a12

        <primitive: #value:value:value:value:value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9 value: a10 value: a11 value: a12 value: a13

        <primitive: #value:value:value:value:value:value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9 value: a10 value: a11 value: a12 value: a13 value: a14

        <primitive: #value:value:value:value:value:value:value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9 value: a10 value: a11 value: a12 value: a13 value: a14 value: a15

        <primitive: #value:value:value:value:value:value:value:value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9 value: a10 value: a11 value: a12 value: a13 value: a14 value: a15 value: a16

        <primitive: #value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9 value: a10 value: a11 value: a12 value: a13 value: a14 value: a15 value: a16 value: a17

        <primitive: #value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9 value: a10 value: a11 value: a12 value: a13 value: a14 value: a15 value: a16 value: a17 value: a18

        <primitive: #value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9 value: a10 value: a11 value: a12 value: a13 value: a14 value: a15 value: a16 value: a17 value: a18 value: a19

        <primitive: #value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9 value: a10 value: a11 value: a12 value: a13 value: a14 value: a15 value: a16 value: a17 value: a18 value: a19 value: a20

        <primitive: #value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9 value: a10 value: a11 value: a12 value: a13 value: a14 value: a15 value: a16 value: a17 value: a18 value: a19 value: a20 value: a21

        <primitive: #value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9 value: a10 value: a11 value: a12 value: a13 value: a14 value: a15 value: a16 value: a17 value: a18 value: a19 value: a20 value: a21 value: a22

        <primitive: #value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9 value: a10 value: a11 value: a12 value: a13 value: a14 value: a15 value: a16 value: a17 value: a18 value: a19 value: a20 value: a21 value: a22 value: a23

        <primitive: #value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9 value: a10 value: a11 value: a12 value: a13 value: a14 value: a15 value: a16 value: a17 value: a18 value: a19 value: a20 value: a21 value: a22 value: a23 value: a24

        <primitive: #value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9 value: a10 value: a11 value: a12 value: a13 value: a14 value: a15 value: a16 value: a17 value: a18 value: a19 value: a20 value: a21 value: a22 value: a23 value: a24 value: a25

        <primitive: #value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9 value: a10 value: a11 value: a12 value: a13 value: a14 value: a15 value: a16 value: a17 value: a18 value: a19 value: a20 value: a21 value: a22 value: a23 value: a24 value: a25 value: a26

        <primitive: #value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9 value: a10 value: a11 value: a12 value: a13 value: a14 value: a15 value: a16 value: a17 value: a18 value: a19 value: a20 value: a21 value: a22 value: a23 value: a24 value: a25 value: a26 value: a27

        <primitive: #value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9 value: a10 value: a11 value: a12 value: a13 value: a14 value: a15 value: a16 value: a17 value: a18 value: a19 value: a20 value: a21 value: a22 value: a23 value: a24 value: a25 value: a26 value: a27 value: a28

        <primitive: #value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9 value: a10 value: a11 value: a12 value: a13 value: a14 value: a15 value: a16 value: a17 value: a18 value: a19 value: a20 value: a21 value: a22 value: a23 value: a24 value: a25 value: a26 value: a27 value: a28 value: a29

        <primitive: #value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9 value: a10 value: a11 value: a12 value: a13 value: a14 value: a15 value: a16 value: a17 value: a18 value: a19 value: a20 value: a21 value: a22 value: a23 value: a24 value: a25 value: a26 value: a27 value: a28 value: a29 value: a30

        <primitive: #value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9 value: a10 value: a11 value: a12 value: a13 value: a14 value: a15 value: a16 value: a17 value: a18 value: a19 value: a20 value: a21 value: a22 value: a23 value: a24 value: a25 value: a26 value: a27 value: a28 value: a29 value: a30 value: a31

        <primitive: #value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value: domain: Block>
];

protocol: #'system primitives' method:
[## value: a1 value: a2 value: a3 value: a4 value: a5 value: a6 value: a7 value: a8 value: a9 value: a10 value: a11 value: a12 value: a13 value: a14 value: a15 value: a16 value: a17 value: a18 value: a19 value: a20 value: a21 value: a22 value: a23 value: a24 value: a25 value: a26 value: a27 value: a28 value: a29 value: a30 value: a31 value: a32

        <primitive: #value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value: domain: Block>
]}}