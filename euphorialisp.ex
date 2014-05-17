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

function SetCar(object obj, object val)
  object data = Data(obj)
  data[1] = val
  put(obj, "data", data)
  return val
end function

function SetCdr(object obj, object val)
  object data = Data(obj)
  data[2] = val
  put(obj, "data", data)
  return val
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

function nreverse(object lst)
  object ret = kNil
  while TagEq(lst, "cons") do
    object tmp = Cdr(lst)
    SetCdr(lst, ret)
    ret = lst
    lst = tmp
  end while
  return ret
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
    return readList(str[2..$])
  elsif str[1] = kQuote then
    sequence tmp = read(str[2..$])
    return {makeCons(makeSym("quote"), makeCons(tmp[1], kNil)), tmp[2]}
  end if
  return readAtom(str)
end function

function readList(sequence str)
  object ret = kNil
  while 1 do
    str = skipSpaces(str)
    if length(str) = 0 then
      return {makeError("unfinished parenthesis"), ""}
    elsif str[1] = kRPar then
      exit
    end if
    sequence tmp = read(str)
    object elm = tmp[1]
    sequence next = tmp[2]
    if TagEq(elm, "error") then
      return {elm, ""}
    end if
    ret = makeCons(elm, ret)
    str = next
  end while
  return {nreverse(ret), str[2..$]}
end function

function printObj(object obj)
  if TagEq(obj, "num") or TagEq(obj, "sym") or TagEq(obj, "nil") then
    return to_string(Data(obj))
  elsif TagEq(obj, "error") then
    return "<error: " & Data(obj) & ">"
  elsif TagEq(obj, "cons") then
    return printList(obj)
  elsif TagEq(obj, "subr") or TagEq(obj, "expr") then
    return "<" & Tag(obj) & ">"
  end if
  return "<unknown>"
end function

function printList(object obj)
  sequence ret = ""
  integer first = 1
  while TagEq(obj, "cons") do
    if first then
      first = 0
    else
      ret = ret & " "
    end if
    ret = ret & printObj(Car(obj))
    obj = Cdr(obj)
  end while
  if TagEq(obj, "nil") then
    return "(" & ret & ")"
  end if
  return "(" & ret & " . " & printObj(obj) & ")"
end function

puts(STDOUT, "> ")
object line = gets(STDIN)
while 1 do
  if atom(line) then
    exit
  end if
  sequence tmp = read(line)
  puts(STDOUT, printObj(tmp[1]) & "\n")
  puts(STDOUT, "> ")
  line = gets(STDIN)
end while
