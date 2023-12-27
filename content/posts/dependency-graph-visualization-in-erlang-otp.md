---
title: "Dependency Graph Visualization in Erlang/OTP"
date: 2023-10-20T19:09:03+09:00
draft: false
tags: ["Erlang", "OTP"]
---

This will be a short one as there is not much to say however I want to keep my findings somewhere so here it is.

## TLDR
```erl
#!/usr/bin/env escript
% -*- erlang -*-

main(_) ->
    xref:start(server),
    xref:add_directory(server, "_build/default/lib", [{recurse, true}]),
    {ok, Deps} = xref:q(server, "E ||| V"),
    xref:stop(server),

    FromTo = lists:map(
        fun(X) ->
            {{From, _, _}, {To, _, _}} = X,
            {From, To}
        end,
        Deps
    ),

    FromToDifferent = lists:filter(
        fun(X) ->
            {From, To} = X,
            From =/= To
        end,
        FromTo
    ),

    FromToUnique = lists:uniq(FromToDifferent),

    LinksText = lists:foldl(
        fun(X, Acc) ->
            {From, To} = X,
            FromString = atom_to_list(From),
            ToString = atom_to_list(To),

            case string:prefix(ToString, "$") of
                nomatch ->
                    string:join(
                        [Acc, "\t", FromString, " -> ", ToString, "\n"], ""
                    );
                _ -> Acc
            end
        end,
        "",
        FromToUnique
    ),

    Output = string:join(["strict digraph {\n", LinksText, "}\n"], ""),
    ok = file:write_file("./graph.dot", Output),
    os:cmd("dot -x -Goverlap=scale -Tpng ./graph.dot -o ./graph.png"),
    os:cmd("firefox ./graph.png").
```

## Find dependencies
To identify dependencies, we will use the `xref` module that comes with the Erlang standard library.

First, create an `xref` server by typing the following:

```erl
xref:start(server).
```

Next, provide a directory for analysis. 

In this example, I will use the default build directory, but you can specify the directory of a specific version.

```erl
xref:add_directory(server, "_build/default/lib", [{recurse, true}]).
```

Finally, query the dependencies.

In this case my query is `E ||| V` to find the subset of calls to and from any of the vertices. For more information, refer to the [xref documentation](https://www.erlang.org/doc/man/xref)

```erl
{ok, Deps} = xref:q(server, "E ||| V").
```

## Filter and transform the dependency list
First, transform the tuples given by `xref` to a simpler `{from, to}` tuple.
```erl
FromTo = lists:map(
    fun(X) ->
        {{From, _, _}, {To, _, _}} = X,
        {From, To}
    end,
    Deps
).
```

Next, remove the modules that point to themselves, as it is not interesting in this case. Keep only unique tuples to avoid repetition.
```erl
FromToDifferent = lists:filter(
    fun(X) ->
        {From, To} = X,
        From =/= To
    end,
    FromTo
),
FromToUnique = lists:uniq(FromToDifferent).
```

## Create the output text
Transform the list into a string that adheres to the `graphviz` format. This format allows you to later create a `png` or `pdf` of the graph.
```erl
LinksText = lists:foldl(
    fun(X, Acc) ->
        {From, To} = X,
        FromString = atom_to_list(From),
        ToString = atom_to_list(To),

        % '$' breaks xgraphviz
        case string:prefix(ToString, "$") of
            nomatch ->
                string:join(
                    [Acc, "\t", FromString, " -> ", ToString, "\n"], ""
                );
            _ -> Acc
        end
    end,
    "",
    FromToUnique
),
Output = string:join(["strict digraph {\n", LinksText, "}\n"], "").
```

Finally, write the `Output` variable to a file, launch the `graphviz` command, and use `firefox` to visualize the generated image.
```erl
ok = file:write_file("./graph.dot", Output),
os:cmd("dot -x -Goverlap=scale -Tpng ./graph.dot -o ./graph.png"),
os:cmd("firefox ./graph.png").
```
The `x -Goverlap=scale` flags are used to improve the output file by giving more space between nodes. However, as you will see below, the readability is still not perfect.

## Limitations
While writing this script, I discovered that some elements are missing. At the beginning, I thought I had made a mistake, but it turned out to be a limitation. This limitation is that it is unable to extract information from unresolved calls such as `spawn` or `apply`.

## Result
![Simple dependency graph](/images/simple-dependency-graph.png)

![Complex dependency graph](/images/complex-dependency-graph.png)
