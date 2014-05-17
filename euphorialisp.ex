include std/convert.e
include std/io.e
include std/map.e

atom kLPar = '('
atom kRPar = ')'
atom kQuote = '\''

function L(sequence tag, object data)
  map obj = new()
  put(obj, "tag", tag)
  put(obj, "data", data)
  return obj
end function

function Tag(object obj)
  return get(obj, "tag")
end function

function TagEq(object obj, sequence str)
  return equal(Tag(obj), str)
end function

function Data(object obj)
  return get(obj, "data")
end function

function DataN(object obj, integer n)
  object data = Data(obj)
  return data[n]
end function

function Car(object obj)
  return DataN(obj, 1)
end function

function Cdr(object obj)
  return DataN(obj, 2)
end function

function Args(object obj)
  return DataN(obj, 1)
end function

function Body(object obj)
  return DataN(obj, 2)
end function

function Env(object obj)
  return DataN(obj, 3)
end function

object kNil = L("nil", "nil")

function safeCar(object obj)
  if TagEq(obj, "cons") then
    return Car(obj)
  end if
  return kNil
end function

function safeCdr(object obj)
  if TagEq(obj, "cons") then
    return Cdr(obj)
  end if
  return kNil
end function

function makeError(sequence str)
  return L("error", str)
end function

map sym_table = new()
function makeSym(sequence str)
  if equal(str, "nil") then
    return kNil
  elsif not has(sym_table, str) then
    put(sym_table, str, L("sym", str))
  end if
  return get(sym_table, str)
end function

function makeNum(integer num)
  return L("num", num)
end function

function makeCons(object a, object d)
  return L("cons", {a, d})
end function

function makeSubr(sequence fn)
  return L("subr", fn)
end function

function makeExpr(object args, object env)
  return L("expr", {safeCar(args), safeCdr(args), env})
end function

function isSpace(atom c)
  return c = ' ' or c = '\t' or c = '\r' or c = '\n'
end function

function isDelimiter(atom c)
  return c = kLPar or c = kRPar or c = kQuote or isSpace(c)
end function

function skipSpaces(sequence str)
  for i = 1 to length(str) do
    if not isSpace(str[i]) then
      return str[i..$]
    end if
  end for
  return ""
end function

function makeNumOrSym(sequence str)
  integer n = to_integer(str)
  if equal(str, to_string(n)) then
    return makeNum(n)
  end if
  return makeSym(str)
end function

function readAtom(sequence str)
  sequence next = ""
  for i = 1 to length(str) do
    if isDelimiter(str[i]) then
      next = str[i..$]
      str = str[1..i-1]
      exit
    end if
  end for
  return {makeNumOrSym(str), next}
end function

function read(sequence str)
  str = skipSpaces(str)
  if length(str) = 0 then
    return {makeError("empty input"), ""}
  elsif str[1] = kRPar then
    return {makeError("invalid syntax: " & str), ""}
  elsif str[1] = kLPar then
    return {makeError("noimpl"), ""}
  elsif str[1] = kQuote then
    return {makeError("noimpl"), ""}
  end if
  return readAtom(str)
end function

puts(STDOUT, "> ")
object line = gets(STDIN)
while 1 do
  if atom(line) then
    exit
  end if
  sequence tmp = read(line)
  puts(STDOUT, to_string(Data(tmp[1])) & "\n")
  puts(STDOUT, "> ")
  line = gets(STDIN)
end while
