% Test Suite: Cell Dereference Unifications & Expressions
% Covers: Dereference unification, double indirection, expression dereferencing

% 1. Simple Dereference Unification
local C X in
    {NewCell X C}
    @C = 5
    {Browse X}   % Should print 5
    {Browse @C}  % Should print 5
end

% 2. Double Indirection
local C D Y in
    {NewCell Y D}
    {NewCell D C}
    {Browse @@C}  % Should print _ (unbound)
    @@C = 10
    {Browse Y}    % Should print 10
    {Browse @@C}  % Should print 10
end

% 3. Expression Dereferencing
local D F in
    {NewCell 10 D}
    fun {F} D end
    {Browse @{F}} % Should print 10
end

% 4. Dereference of NewCell expression in unification
local Z in
    @{NewCell Z} = 42
    {Browse Z}    % Should print 42
end
