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
---@alias Token { kind: "nl", pos: Position }|{ kind: "word", value: string, pos: Position }|{ kind: "symbol", value: string, pos: Position }|{ kind: "number", value: number, pos: Position }
local token = {
    meta = {
        __name = "token"
    }
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
                table.insert(tokens, token.word(word, cpos))
                break
            end
            return nil, ("invalid character %q"):format(get()), cpos
        end
    end

    return tokens
end

local tokens, err = lex([[move up
turn left
]])
if not tokens then
    print(err)
    return
end
for _, token in ipairs(tokens) do
    print(token.kind, token.value or "")
end