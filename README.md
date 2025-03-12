# Outrun

Here's the content of our chat thread:


    James Neil Harton:
        yeah but what if you had a language where you didn’t have to write ash because all the modules have enough metadata to introspect them directly?
        like yeah you’d have to write chunks of ash

    Zach Daniel:
        🤔what would that look like?

    James Neil Harton:
        I have no idea

    Zach Daniel:
        😂
        It would be like “function templates”

    James Neil Harton:
        only one way to find out

    Zach Daniel:
        Or something. I guess that’s kind of like type classes but for the structure of functions? `function classes`?
        God damnit James don’t do this to me haha

    James Neil Harton:
        So my idea with outrun is that I wanted “everything to be traits” where traits are like protocols in Elixir. So you can write something like `def echo(string: String) when String.contains?(“banana”)` and it’s more like duck typing where I don’t care what you pass in as the `string` argument as long as it implements the `String` type
        basically any trait function ending in a `?` has to be side-effect free so it can be used as a guard

    Zach Daniel:
        I like the idea, but it can’t be protocol driven IMO, it has to be done while compiling as part of the type system
        Like not based on values

    James Neil Harton:
        yeah, it still has to be statically typed
        but the constraints are that you initially compile assuming that all concrete types match the trait constraints of the calls they’re used for, then you go back again and consolidate the traits which basically get erased in the process

    Zach Daniel:
        Right

    James Neil Harton:
        that may be a dumb idea but that’s kind of where my head was at

    Zach Daniel:
        Yeah, I mean it could work. Its an interesting question of what I would want out of a language to give me ash-y stuff w/o needing, well, this massive mondo framework

    James Neil Harton:
        I still think you’d need a massive mondo framework to do all the “derive the rest” stuff
        but I want the language to “model your domain”

    Zach Daniel:
        🤔
        You’ve got me thinking for sure

    James Neil Harton:
        imagine
    ```
    defstruct UserRegisterWithPasswordInput{email: String, name?: String, password: String} do
     impl ActionInput …
    end 

    defstruct User{id: UUID, email: String, name: String} do

     def register_with_password(input: UserRegisterWithPasswordInput): Result<User, SomeError>

      impl ActionResult
    end
    ```
    If the code itself is introspectable you have enough information to make something interesting happen
        I mean that’s just one of many different ways it could work
        I also had an idea of like a type algebra kind of thing where you can say “a number is (anything that implements ToNativeInteger && BinaryAddition, BinaryMultiplication && ..) or (anything that implements ToNativeFloat && BinaryAddititon && …)`

    Zach Daniel:
        Yeah, that sounds pretty cool
        I’m having trouble converging on what I’d want out of a new language, but it has something to do with being able to give types actual functionality
        Kind of like in `Ash` how `Ash.Type.Integer` has callbacks

    James Neil Harton:
        so then you have `deftrait Number((ToNativeInteger || ToNativeFloat) && BinaryAddition …)`

    Zach Daniel:
        I think that might be what you’re saying effectively?
        But not on a value, on the type itself
        That affect how type resolution happens

    James Neil Harton:
        yeah because a trait has functions that are implemented on it _and_ can be optionally overridden (like in rust or like in `use Iter.Iterator.DefaultImplementation`

    Zach Daniel:
        The other thing is like how actions work for example, they are just data. So perhaps a “value” format for real types
        i.e `{name: String}` where `String` is both a type and a value and so I can switch on a type at runtime
        So `validate(value, type)` is `type.validate(value)`. Its not really making sense to me yet

    James Neil Harton:
        right

    Zach Daniel:
        Also FWIW I love the name and theming of outrun ❤️ 

    James Neil Harton:
        so traits are also types

    Zach Daniel:
        I can already imagine the website haha

    James Neil Harton:
        so a trait may in compiler land turn into `deftype MyTrait(Unit)` where it basically has a unit value 

    Zach Daniel:
        I can’t tell if this cough medicine is making me have a more open mind or a more dumb one 😂

    James Neil Harton:
        why not both?

    Zach Daniel:
        Yes
        **yis

    James Neil Harton:
        but imagine if you will a world where you can implement `BinaryAddition` for a type and then be allowed to call `myvalue + someothervalue` and have it just work!?

    Zach Daniel:
        Aw man

    James Neil Harton:
        no more `DateTime.cmp`

    Zach Daniel:
        You kinda just sold me but maybe not in the way intended

    James Neil Harton:
        for example

    Zach Daniel:
        What if you could implement a `to_sql` for your type that operates at a syntactic level

    James Neil Harton:
        right
        technically you could do that with Ash now `AshSql.to_sql(record, dialect: AshPostgres)`
        if `AshSql` were a protocol I mean

    Zach Daniel:
        It’s interesting because I think that Ash resources for the most part are effectively as concise as application logic can be. Like *for the most part*. Plenty of exceptions. So the real question is how the language can help me define those things and then how it can help me interpret it. I think a few things that come to mind is:

    1. A built in data specification language, i.e first-class DSLs
    2. The expression stuff we just talked about? Native transpilation? Gleam does this for js.
    3. Perhaps some concept for projections from data structures matching a given specification. Like defining an interface and saying “anything that implements this data specification can be provided to this function"

    James Neil Harton:
        yeah but even better - if your language is built _on top_ of tree sitter then you can do:
    ```
    impl ToSql do
      def to_sql(query) do
        ~SQL”””
          SELECT * FROM …
        “””
      end
    end
    ```
    and have all the SQL language server stuff work in your editor

    Zach Daniel:
        Typescript has interfaces that you can do that kind of thing with. i.e “I accept anything that has a property X of type Y”
        I don’t understand the tree sitter part of this yet

    James Neil Harton:
        yeah, except we never do structure field access without - instead we say it has to implement some trait which provides access to the fields
        so you never say “anything that has property X of type Y” but you say “anything that has get me X of type Y”

    Zach Daniel:
        I feel like the first order of business would be to syntactic sugar that away TBH
        Like for maps etc.

    James Neil Harton:
        sure

    Zach Daniel:
        But yes

    James Neil Harton:
        maps are shit though
        like sometimes you can’t avoid them
        but please use a concrete type

    Zach Daniel:
        Sure, but if we do the “structs are just maps” thing you get some nice properties

    James Neil Harton:
        well structs can derive map-like behaviour

    Zach Daniel:
        Like `@type thing(map: %{x: String}) :: …` or w/e
        Right
        So its really just that syntax in type constraints that has to desugar to “getKey” which both maps and structs implement

    James Neil Harton:
        where `Map` is a protocol and `Outrun.Native.HashMap` may be a concrete implementation of it but so may be `MyCrazyLinkedListOfTuples`

    Zach Daniel:
        You’re looking to build a BEAM language right?

    James Neil Harton:
        fuck no

    Zach Daniel:
        Aw man that’s like a hard pass for me dude
        I’m not deploying stuff w/o the beam
        I could live w/o Elixir

    James Neil Harton:
        I wanted to use something like the Rust bastion crate to get actor behavior

    Zach Daniel:
        Hrm...I dunno.
        I’d have to be sold pretty hard on that actual behavior.

    James Neil Harton:
        like “in the spirit of the beam” but without all the baggage - until we make our own that is

    Zach Daniel:
        Why not just use Rust then? Rust has most of this stuff in its type system

    James Neil Harton:
        because rust is too low level

    Zach Daniel:
        They have traits etc.

    James Neil Harton:
        and it’s objects
        and it’s immutable and stuff
        but also why not make BEAM a compilation target as a first gut-check of the programming model?

    Zach Daniel:
        I mean, if we’re just doing this for fun, then I say we build our own runtime
        Do what firefly/lumen couldn’t
        And make WASM a target

    James Neil Harton:
        exactly right

    Zach Daniel:
        God damnit dude I don’t have time for this but it sounds really fun and like I’d learn a whole lot 😊 
        Have you considered LLVM?

    James Neil Harton:
        yes, I have spent a long time down the LLVM rabbit hole
        but then there’s the crane lift compiler
        which has native and wasm targets
        https://cranelift.dev/

    Zach Daniel:
        I’m pretty out of my depth in this regard. I wrote a compiler back in the day w/ LLVM for a toy language, but this was 10 years ago and I don’t remember much.

    James Neil Harton:
        yeah it’s been a while for me too

    Zach Daniel:
        So if we’re “starting from scratch”, I feel like a lot of syntactic decisions have to be made

    James Neil Harton:
        yes

    Zach Daniel:
        I also can’t really live w/o significant metaprogramming 😂

    James Neil Harton:
        I agree with you
        which is why I think we need to start by writing an interpreter before a compiler

    Zach Daniel:
        So it needs to have Elixir’s “compile time is just a runtime” properties.
        Right

    James Neil Harton:
        because we have to be able to evaluate code as part of the compiler it will have to be bootstrapped in itself

    Zach Daniel:
        We also still want addressable modules etc. 
        I can live w/o `do/end` though

    James Neil Harton:
        yup, I think type names are global constants like beam/ruby

    Zach Daniel:
        What do you think about having no positional arguments?
        Everything is named

    James Neil Harton:
        yes,. everything is named and typed

    Zach Daniel:
        So you’re down with `add(1, 2)` not being a thing?

    James Neil Harton:
        like maybe we can add type inference later, but it’s a whole lot easier to start without it

    Zach Daniel:
        `add(l: 1, r: 2)`

    James Neil Harton:
        yes
        but really `BinaryAddition.add(l: 1, r: 2)`

    Zach Daniel:
        Word. It’s pretty wild but I’m down to try it out. Easy enough to back *out* of that

    James Neil Harton:
        I’m copying this chat thread into markdown

    Zach Daniel:
        Should we move this chat somewhere else for archivability?
