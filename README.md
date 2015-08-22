# JamesBot2

Version 2 of my chat bot, with all new goodness!

## Highlights

- Totally typesafe route matching (making use of `DataKinds` and `TypeFamilies`)
- Can ask for the result of any parsers used in the routes, allowing the parsers to take on some of the heavy lifting (for example, parsing date/time relaetd information into proper UTCTime objects).
- Top level parsers can optionally be functions of `state -> Parser`, allowing them to be passed useful information (such as the current time). At the same time, out-of-the-box parsers can be used in routes without wrapping them.

## Routing

An example route definition used looks like

```haskell
    pBotName    <..>
    pS "remind" <..>
    var pName   <..>
    var (pMaybe $ pS "(" *> pUntil (pS ")")) <..>
    var pTime   <..>
    pS "to"     <..>
    var pRest
```

It's a little messier than Id like but it serves its purpose. A message matches the route this parser is associated with if it matches the sequence of inputs (corresponding to the above line by line):

1. bots name
2. the string "remind"
3. a namelike word, and pass it to the associated function (`var` does this)
4. capture everything until ")", passing it to the associated function
5. capture something that looks like a date/time, passing it to the associated function
6. the string "to"
7. capture everyting else, providing to the associated function

The associated function then expects to be provided the result of each parser that has been prefixed with the word `var`. This is enforced at compile time. Either the route matches and every parser that is required can hand its output to the function, or the route is rejected and we try the next one.

This approach allows parsers to take on the heavy lifting. For instance, the `pTime` parser will do its best to extract anything along the lines of:

```
5 minutes and 3 seconds
next week on thursday
10/12/2017 10pm
10:32:12
June 21st 2016
3 months
one year from now
18th December 10am
```

and others, returning the result as a `UTCTime` object. This means that the route doesn't have to think about any of that, and can get straight to business using the provided time in the reminder system.

For this to work, the `pTime` parser is actually a function of `RoutesInput -> Parser UTCTime`, but some type class magic allows that to be used much like the other out-of-the-box parsers (I use _Attoparsec_ for my parsing needs since it is fast, and I prefer the semantics of operations like `<|>` to Parsec).

## Websockets

I wrote a simple wrapper around the websocket implementation I used to automatically encode and decode JSON of the correct types, and handle failure. This was a satisfying use of Haskells type system; by keeping the websocket server quite vague (just telling it that it will be possible to encode/decode things, but not to what type), I can simply "expect" some type to be handed back, and the socket wrapper will, due to the type inference, try and decode to that type when a message comes in, ignoring it if decoding fails. This lets me handle incoming messages and ignore naff ones (well, log an error) transparently. I can change the expected types down the road and it'll all just keep working with no changes to the socket wrapper.

I also wrote a very quick channel implementation to act as an interface to send and receive messages on (all of 7 lines of not-very-compact code) which offers up a pair of `read` and `write` back, and blocks if the consumer/producer on the other end is slow so that we don't end up ignoring any messages.

## Persistance

Not wanting to do anything complex, I wrote a very simple persistance handler that can be pointed at any `MVar` containing a type that can be encoded/decoded to/from JSON (since I was already using the format elsewhere). It checks to see whether the `MVar` contents change and, when they do, writes to some file. On init, it fille the MVar with the decoded content of the same file. Very simple (less than 100loc all in) but it has worked very well for me.

That's pretty much everything of any interest. Have a look/fork the code if you wish!

