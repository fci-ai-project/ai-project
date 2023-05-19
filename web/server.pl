:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_error)).
:- use_module(library(http/websocket)).

:- http_handler(root(.), index, []).
:- http_handler(root(assets), serve_front_assets, [prefix]).
:- http_handler(root(play), http_upgrade_to_websocket(handle_play, []), [spawn([])]).

:- ['engine/connect_n'].
:- ['engine/game'].
:- ['engine/algorithms'].


port(2002).

create_message(Type, Data, _{ data: _{ type: Type, data: Data }, format: json, opcode: text }).

/*
    Message types: move, state, invalid_move, init
    move data: number
    state data: 2d matrix
    invalid_move data: number
    init data: {
        rows: number greater than 0,
        columns: number greater than 0,
        color: 'r|b',
        winning_factor: number greater than 0,
        algorithm: 'minimax|abp',
        max_plies: number greater than 0,
        turn: 'human|computer'
    }
*/

get_move(WebSocket, Move) :-
    create_message(turn, human, Message),
    ws_send(WebSocket, json(Message)),
    ws_receive(WebSocket, Response, [format(json)]), % TODO: handle socket close
    Response.data.type = move,
    Move = Response.data.data.

announce_state(WebSocket, State) :-
    create_message(state, State, Message),
    ws_send(WebSocket, json(Message)).

announce_invalid_move(WebSocket, Move) :-
    create_message(invalid_move, Move, Message),
    ws_send(WebSocket, json(Message)).

announce_winner(WebSocket, Winner) :-
    create_message(winner, Winner, Message),
    ws_send(WebSocket, json(Message)).

index(Request) :-
    http_reply_file('web/front/index.html', [], Request).

serve_front_assets(Request) :-
    member(path(Path), Request),
    atom_concat('web/front', Path, FilePath),
    exists_file(FilePath),
    !,
    http_reply_file(FilePath, [], Request).

serve_front_assets(Request) :-
    http_404([], Request).

choose_color("r", Color, OppositeColor) :- red_piece(Color), blue_piece(OppositeColor).
choose_color("b", Color, OppositeColor) :- blue_piece(Color), red_piece(OppositeColor).

choose_algo_util("minimax", minimax_algo_util).
choose_algo_util("abp", abp_algo_util).

choose_turn("human", human).
choose_turn("computer", computer).

handle_play(WebSocket) :-
    ws_receive(WebSocket, Message, [format(json)]),
    Message.data.type = "init",
    _{
        rows: Rows,
        columns: Columns,
        color: ColorResponse,
        winning_factor: WinningFactor,
        algorithm: AlgorithmResponse,
        max_plies: MaxPlies,
        turn: TurnResponse
    } = Message.data.data,
    form_board(Rows, Columns, Board),
    choose_color(ColorResponse, HumanColor, ComputerColor),
    Players = _{
        human: HumanColor,
        computer: ComputerColor
    },
    state_util(WinningFactor, StateUtil),
    choose_algo_util(AlgorithmResponse, AlgoUtilBuilder),
    call(AlgoUtilBuilder, StateUtil, MaxPlies, AlgoUtil),
    IOUtil = _{
        get_move: get_move(WebSocket),
        announce_state: announce_state(WebSocket),
        announce_invalid_move: announce_invalid_move(WebSocket),
        announce_winner: announce_winner(WebSocket)
    },
    choose_turn(TurnResponse, Turn),
    play(Board, Turn, Players, StateUtil, IOUtil, AlgoUtil, _).

serve() :-
    port(Port),
    http_server(http_dispatch, [port(Port)]).

stop() :-
    port(Port),
    http_stop_server(Port, []).
