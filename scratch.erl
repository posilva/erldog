-module(scratch).
-export([start/0]).
start() ->
    application:load(jsx),
    Metric = "metric.name",
    Points = [[<<"123445">>,<<"21">>],[<<"12321323">>,<<"31">>]],
    Tags = [<<"a">>,<<"b">>,<<"sads">>],

    Struct = [
        {<<"series">>, [[
            {<<"metric">>, list_to_binary(Metric) },
            {<<"points">>, [
                [1212121,1]
            ]},
            {<<"type">>, <<"gauge">> },
            {<<"host">>, <<"miniclip.com">>},
            {<<"tags">>, [
                <<"tag1">>,
                <<"tag2">>
            ]}
        ]]}
    ],
    JSON = jsx:encode(Struct).
