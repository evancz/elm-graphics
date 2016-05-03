# Elm's Original Graphics Library

Elm actually started as a small set of visual *elements*, not a language. That is partly why I chose the name Elm out of all the nice tree names out there. This library is that original set of graphical primitives.

In particular, the `Graphics.Collage` module is really nice for free-form graphics and projects like [McMaster Outreach](http://outreach.mcmaster.ca/) have used it successfully for teaching programming in a way that is fun, easy, and interactive.

If you are working on professional web apps, it is better to stick to HTML and SVG for now. It is true that `Graphics.Collage` is backed by `<canvas>`, but I expect we will eventually have an API specifically targetted at canvas that covers more features. So if you need super advanced canvas stuff, it may be better to manage that in JavaScript and communicate to Elm with ports.