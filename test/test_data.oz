% Test Suite: Data Structures & State
% Covers: Lists, Records, Cells, Cycles, Browse Formats

local List Rec Cell Res CyclicList CyclicRec in
    % 1. Lists
    List = 10 | 20 | nil
    {Browse List} % _N: 10 | 20 | nil

    % 2. Records
    Rec = person(name: 'Alice' age: 30)
    {Browse Rec}

    % 3. Cells (State)
    {NewCell 0 Cell}
    {Exchange Cell Res 5}
    {Browse Res}   % 0
    {Browse @Cell} % 5
    Cell := 10     % Syntactic sugar
    {Browse @Cell} % 10

    % 4. Cyclic Structures
    % List Cycle: (C1) 1 | (C2) 2 | C1 | C2
    local X Y in
        X = 1|Y
        Y = 2|X
        CyclicList = X
    end
    {Browse CyclicList}

    % Record Cycle: (C1) a(x:C1 y:C1)
    local R in
        R = a(x:R y:R)
        CyclicRec = R
    end
    {Browse CyclicRec}
end
%show mutable
