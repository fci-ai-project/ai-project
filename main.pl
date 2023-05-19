:- prolog_load_context(directory, D), working_directory(_, D).

:- ['web/server'].

:- serve(), www_open_url('http://localhost:2002').
