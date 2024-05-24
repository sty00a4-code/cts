---@param s string
---@param ... string
function string.split(s, ...)
    local seps = { ... }
    local temp, t = "", {}
    local idx = 1
    while idx <= #s do
        local match
        for _, sep in ipairs(seps) do
            if s:sub(idx, idx + #sep - 1) == sep then
                match = sep
                break
            end
        end
        if match then
            if #temp > 0 then
                table.insert(t, temp)
                temp = ""
            end
            idx = idx + #match
        else
            temp = temp .. s:sub(idx, idx)
            idx = idx + 1
        end
    end
    if #temp > 0 then
        table.insert(t, temp)
    end
    return t
end

local position_mt = {
    __name = "position"
}
local function postion(ln, col)
    ---@class Position : { ln: integer, col: integer }
    return setmetatable({
        ln = ln,
        col = col,
    }, position_mt)
end
---@alias Token { kind: "nl", pos: Position }|{ kind: "word", value: string, pos: Position }|{ kind: "kw", value: string, pos: Position }|{ kind: "symbol", value: string, pos: Position }|{ kind: "number", value: number, pos: Position }
local token = {
    meta = {
        __name = "token",
        __eq = function (self, other)
            if type(other) == "table" then
                if other.value then
                    return self.kind == other.kind and self.value == other.value
                else
                    return self.kind == other.kind
                end
            elseif type(other) == "string" then
                local parts = other:split(":")
                local kind, value = parts[1], parts[2]
                if value then
                    return self.kind == kind and self.value == value
                else
                    return self.kind == kind
                end
            else
                return false
            end
        end
    },
    kws = {
        ["end"] = true,
        ["const"] = true,
        ["global"] = true,
        ["procedure"] = true,
        ["var"] = true,
        ["repeat"] = true,
        ["while"] = true,
        ["break"] = true,
        ["if"] = true,
        ["else"] = true,
        ["match"] = true,
        ["return"] = true,
    },
}
---@param pos Position
---@return Token
function token.nl(pos)
    return setmetatable({
        kind = "nl",
        pos = pos,
    }, token.meta)
end
---@param word string
---@param pos Position
---@return Token
function token.word(word, pos)
    return setmetatable({
        kind = "word",
        value = word,
        pos = pos,
    }, token.meta)
end
---@param kw string
---@param pos Position
---@return Token
function token.kw(kw, pos)
    return setmetatable({
        kind = "kw",
        value = kw,
        pos = pos,
    }, token.meta)
end
---@param symbol string
---@param pos Position
---@return Token
function token.symbol(symbol, pos)
    return setmetatable({
        kind = "symbol",
        value = symbol,
        pos = pos,
    }, token.meta)
end
---@param number number
---@param pos Position
---@return Token
function token.number(number, pos)
    return setmetatable({
        kind = "number",
        value = number,
        pos = pos,
    }, token.meta)
end
---@param string number
---@param pos Position
---@return Token
function token.string(string, pos)
    return setmetatable({
        kind = "string",
        value = string,
        pos = pos,
    }, token.meta)
end

---@param text string
---@return Token[]?, string?, Position?
local function lex(text)
    local ln, col, idx = 1, 1, 1
    ---@return string
    local function get()
        return text:sub(idx, idx)
    end
    ---@return Position
    local function pos()
        return postion(ln, col)
    end
    local function advance(amount)
        amount = amount ~= nil and amount or 1
        if amount < 1 then
            return
        end
        for _ = 1, amount do
            if get() == "\n" then
                ln = ln + 1
                col = 1
            else
                col = col + 1
            end
            idx = idx + 1
        end
    end
    ---@param patt string
    ---@return string?
    local function match(patt)
        local match = text:match(patt, idx)
        if match then
            if match == text:sub(idx, idx + #match - 1) then
                return match
            end
        end
    end

    ---@type Token[]
    local tokens = {}
    while idx <= #text do
        do
            local ws = match("[ \r\t]+")
            advance(ws and #ws or 0)
        end
        if idx > #text then
            break
        end
        local cpos = pos()
        while true do
            if get() == "\n" then
                advance()
                table.insert(tokens, token.nl(cpos))
                break
            end
            local number = match("%d+.%d*") or match("%d+")
            if #(number or "") > 0 then
                advance(#number)
                ---@diagnostic disable-next-line: param-type-mismatch
                table.insert(tokens, token.number(tonumber(number), cpos))
                break
            end
            local word = match("[%w%d_]+")
            if #(word or "") > 0 then
                advance(#word)
                ---@diagnostic disable-next-line: param-type-mismatch
                table.insert(tokens, token.kws[word] and token.kw(word, pos) or token.word(word, cpos))
                break
            end
            return nil, ("invalid character %q"):format(get()), cpos
        end
    end

    return tokens
end

local ast = {
    metas = {
        parser = {
            __name = "parser",
        },
        program = {
            __name = "ast.program",
        },
        const = {
            __name = "ast.const",
        },
        global = {
            __name = "ast.global",
        },
        procedure = {
            __name = "ast.procedure",
        },
        call = {
            __name = "ast.call",
        },
        _repeat = {
            __name = "ast.repeat",
        },
        _while = {
            __name = "ast.while",
        },
        _break = {
            __name = "ast.break",
        },
        _if = {
            __name = "ast.if",
        },
        match = {
            __name = "ast.match",
        },
        _return = {
            __name = "ast.return",
        },
        word = {
            __name = "ast.word",
        },
        number = {
            __name = "ast.number",
        },
        string = {
            __name = "ast.string",
        },
        binary = {
            __name = "ast.binary",
        },
    },
    parse = {},
    parser = {},
    error = {
        eof = { msg = "unexpected end of file" },
        meta = {
            __name = "parser-error"
        },
    }
}

---@class ConstNode : { kind: "const", ident: WordNode, expr: ExpressionNode, pos: Position }
---@param ident WordNode
---@param expr ExpressionNode
---@param pos Position
---@return ConstNode
function ast.const(ident, expr, pos)
    return setmetatable({
        kind = "const",
        ident = ident,
        expr = expr,
        pos = pos,
    }, ast.metas.const)
end
---@class GlobalNode : { kind: "global", ident: WordNode, expr: ExpressionNode, pos: Position }
---@param ident WordNode
---@param expr ExpressionNode
---@param pos Position
---@return GlobalNode
function ast.global(ident, expr, pos)
    return setmetatable({
        kind = "global",
        ident = ident,
        expr = expr,
        pos = pos,
    }, ast.metas.global)
end
---@class ProcedureNode : { kind: "procedure", ident: WordNode, params: WordNode[], body: StatementNode[], pos: Position }
---@param ident WordNode
---@param params ExpressionNode[]?
---@param body StatementNode[]
---@param pos Position
---@return ProcedureNode
function ast.procedure(ident, params, body, pos)
    return setmetatable({
        kind = "procedure",
        ident = ident,
        params = params,
        body = body,
        pos = pos,
    }, ast.metas.global)
end
---@class ProgramNode : { consts: ConstNode[], globals: GlobalNode[], procedures: ProcedureNode[] }
---@param consts ConstNode[]
---@param globals GlobalNode[]
---@param procedures ProcedureNode[]
---@return ProgramNode
function ast.program(consts, globals, procedures)
    return setmetatable({
        kind = "program",
        consts = consts,
        globals = globals,
        procedures = procedures,
    }, ast.metas.global)
end

---@class CallNode : { kind: "call", head: WordNode, args: ExpressionNode[], pos: Position }
---@param head WordNode
---@param args ExpressionNode[]
---@param pos Position
---@return CallNode
function ast.call(head, args, pos)
    return setmetatable({
        kind = "call",
        head = head,
        args = args,
        pos = pos,
    }, ast.metas.call)
end
---@class RepeatNode : { kind: "repeat", amount: ExpressionNode, body: StatementNode[], pos: Position }
---@param amount ExpressionNode
---@param body StatementNode[]
---@param pos Position
---@return RepeatNode
function ast._repeat(amount, body, pos)
    return setmetatable({
        kind = "repeat",
        amount = amount,
        body = body,
        pos = pos,
    }, ast.metas._repeat)
end
---@class WhileNode : { kind: "while", cond: ExpressionNode, body: StatementNode[], pos: Position }
---@param cond ExpressionNode
---@param body StatementNode[]
---@param pos Position
---@return WhileNode
function ast._while(cond, body, pos)
    return setmetatable({
        kind = "while",
        cond = cond,
        body = body,
        pos = pos,
    }, ast.metas._while)
end
---@class BreakNode : { kind: "break", pos: Position }
---@param pos Position
---@return BreakNode
function ast._break(pos)
    return setmetatable({
        kind = "break",
        pos = pos,
    }, ast.metas._break)
end
---@class IfNode : { kind: "if", cond: ExpressionNode, case: StatementNode[], elseCase: StatementNode[]?, pos: Position }
---@param cond ExpressionNode
---@param case StatementNode[]
---@param elseCase StatementNode[]?
---@param pos Position
---@return IfNode
function ast._if(cond, case, elseCase, pos)
    return setmetatable({
        kind = "if",
        cond = cond,
        case = case,
        elseCase = elseCase,
        pos = pos,
    }, ast.metas._if)
end
---@class MatchNode : { kind: "match", expr: ExpressionNode, cases: { expr: ExpressionNode?, body: StatementNode[] }[], pos: Position }
---@param expr ExpressionNode
---@param cases { expr: ExpressionNode?, body: StatementNode[] }[]
---@param pos Position
---@return MatchNode
function ast.match(expr, cases, pos)
    return setmetatable({
        kind = "match",
        expr = expr,
        cases = cases,
        pos = pos,
    }, ast.metas.match)
end
---@class ReturnNode : { kind: "return", expr: ExpressionNode?, pos: Position }
---@param expr ExpressionNode?
---@param pos Position
---@return ReturnNode
function ast._return(expr, pos)
    return setmetatable({
        kind = "return",
        expr = expr,
        pos = pos,
    }, ast.metas._return)
end
---@alias StatementNode CallNode|RepeatNode|WhileNode|IfNode|MatchNode


---@class WordNode : { kind: "word", word: string, pos: Position }
---@param word string
---@param pos Position
---@return WordNode
function ast.word(word, pos)
    return setmetatable({
        kind = "word",
        word = word,
        pos = pos,
    }, ast.metas.word)
end
---@class NumberNode : { kind: "number", number: number, pos: Position }
---@param number number
---@param pos Position
---@return NumberNode
function ast.number(number, pos)
    return setmetatable({
        kind = "number",
        number = number,
        pos = pos,
    }, ast.metas.number)
end
---@class StringNode : { kind: "string", string: string, pos: Position }
---@param string string
---@param pos Position
---@return StringNode
function ast.string(string, pos)
    return setmetatable({
        kind = "string",
        string = string,
        pos = pos,
    }, ast.metas.string)
end
---@class BinaryNode : { kind: "binary", op: Token, left: ExpressionNode, right: ExpressionNode, pos: Position }
---@param op Token
---@param left ExpressionNode
---@param right ExpressionNode
---@param pos Position
---@return BinaryNode
function ast.binary(op, left, right, pos)
    return setmetatable({
        kind = "binary",
        op = op,
        left = left,
        right = right,
        pos = pos,
    }, ast.metas.binary)
end
---@alias ExpressionNode WordNode|NumberNode|StringNode|BinaryNode|CallNode

function ast.error.expected(expected, token, pos)
    return setmetatable(
        ---@class Error : { msg: string, pos: Position? }    
        {
            msg = ("expected %s, got %s"):format(expected, token),
            pos = pos,
        },
        ast.error.meta
    )
end
function ast.parser.new(tokens)
    return setmetatable(
        ---@class Parser : { tokens: Token[], idx: integer }
        ---@field tokens Token[]
        ---@field idx integer
        ---@field next fun(parser: Parser): Token?
        ---@field peek fun(parser: Parser): Token?
        ---@field expect fun(parser: Parser, token: Token|string): Token?, Error?
        {
            tokens = tokens,
            idx = 1,
            next = ast.parser.next,
            peek = ast.parser.peek,
            expect = ast.parser.expect,
        },
        ast.metas.parser
    )
end
---@param parser Parser
---@return Token?
function ast.parser.next(parser)
    local token = parser.tokens[parser.idx]
    if token then parser.idx = parser.idx + 1 end
    return token
end
---@param parser Parser
---@return Token?
function ast.parser.peek(parser)
    return parser.tokens[parser.idx]
end
---@param parser Parser
---@return Token?, Error?
function ast.parser.expect(parser, expected)
    local token = parser:next()
    if not token then
        return nil, ast.error.eof
    end
    if token ~= expected then
        return nil, ast.error.expected(expected, token, token.pos)
    end
    return token
end

---@param parser Parser
---@return { word: WordNode, expr: ExpressionNode }?, Error?
function ast.parse.assign(parser)
    local word, err = parser:expect("word") if not word then
        return nil, err
    end
    local _, err = parser:expect("symbol:=") if not _ then
        return nil, err
    end
    local expr, err = ast.parse.expression(parser) if not expr then
        return nil, err
    end
    return { word = word, expr = expr }
end
---@param parser Parser
---@return ConstNode?, Error?
function ast.parse.const(parser)
    local token, err = parser:expect("kw:const") if not token then
        return nil, err
    end
    local assign, err = ast.parse.assign(parser) if not assign then
        return nil, err
    end
    local word, expr = assign.word, assign.expr
    return ast.const(word, expr, token.pos)
end
---@param parser Parser
---@return GlobalNode?, Error?
function ast.parse.global(parser)
    local token, err = parser:expect("kw:global") if not token then
        return nil, err
    end
    local assign, err = ast.parse.assign(parser) if not assign then
        return nil, err
    end
    local word, expr = assign.word, assign.expr
    return ast.const(word, expr, token.pos)
end