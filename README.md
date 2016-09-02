concha
=====

![](http://img.carlosgaldino.com/tumblr_nhbiojqLHS1qbtsldo1_1280.jpg)

A consistent hashing library in Erlang.

`concha` allows creating rings with and without virtual nodes, lookup of nodes
based on the given keys, and also viewing and changing the ring structure.

To learn more about consistent hashing read the blog post: <http://blog.carlosgaldino.com/consistent-hashing.html>

Build
-----

    $ rebar3 compile

Usage
-----

```erlang
1> Nodes = ["10.1.2.3", "10.9.5.1", "10.7.5.1", "10.5.5.2"].
["10.1.2.3","10.9.5.1","10.7.5.1","10.5.5.2"]

2> Ring = concha:new(5, Nodes).
%% suppressed output

3> concha:lookup("Venus", Ring).
"10.7.5.1"

4> concha:lookup("Mars", Ring).
"10.1.2.3"

5> concha:members(Ring).
["10.1.2.3","10.5.5.2","10.7.5.1","10.9.5.1"]

6> concha:size(Ring).
20

7> R2 = concha:remove("10.7.5.1", Ring).
%% suppressed output

8> concha:lookup("Venus", R2).
"10.5.5.2"

9> concha:lookup("Mars", R2).
"10.1.2.3"

10> concha:size(R2).
15

11> concha:contains("10.7.5.1", Ring).
true

12> concha:lookup("Pluto", Ring).
"10.7.5.1"

13> R3 = concha:add("10.6.3.1", Ring).
%% suppressed output

14> concha:lookup("Pluto", R3).
"10.6.3.1"

15> concha:size(R3).
25
```

Although the examples use strings for both nodes and keys, the library does not
require them to be of a specific type. They can be any term.

For more information about usage [take a look at the docs](https://hexdocs.pm/concha/concha.html).
