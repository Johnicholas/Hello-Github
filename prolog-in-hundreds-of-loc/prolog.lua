
--Taken from lua-users.org/wiki/SplitJoin
function string:split(separator)
	local answer= {}
	local pattern= string.format("([^%s]+)", separator)
	self:gsub(pattern, function(match) table.insert(answer, match) end)
	return answer
end

--based completely on ioctl.org's prolog-in-javascript
function freeform(rules, query)
	print("Parsing rulesets.");
	rules= rules:split("\n")
	local outr= {}
	for _,rule in ipairs(rules) do
		if rule[0] == "#" or rule == "" then
			-- do nothing
		else
			local parsed_rule= ParseRule(newTokenizer(rule))
			if parsed_rule == nil then
				-- do nothing
			else
				table.insert(outr,parsed_rule)
				parsed_rule:print()
			end
		end
	end

	print("Parsing query.")
	local q= ParseBody(newTokenizer(query))
	if q == nil then
		print("An error occurred parsing the query.\n")
		return
	end
	q= newBody(q)
	q:print()
	print("\n")
	local vs= varNames(q.list)
	prove(renameVariables(q.list, 0), {}, outr, 1, vs)
	
end

function printEnv(env)
	if env == nil then
		print("null")
		return
	end
	local found= false
	for k, v in pairs(env) do
		found=true
		io.write("  " .. k .. "=")
		v:print()
		print()
	end
	if not found then
		print("true")
	end
end

function newAtom(head)
	local answer={
		name= head,
		print= function(self) io.write(self.name) end,
		type= "Atom"
	}
	return answer
end

--print("testing testing")
--printEnv(nil)
--print("one")
--printEnv({[1]= newAtom("foo")})
--print("two")
--printEnv({foo= newAtom("bar"), bar= newAtom("baz")})
--print("three")

--the value of x in a given environment
function value(x, env)
	if x.type=="Term" then
		local l= {}
		for i, v in ipairs(x.partlist.list) do
			table.insert(l, value(v, env))
		end
		return newTerm(x.name, l)
	end
	if x.type == "Variable" then
		local binding= env[x.name]
		if binding == nil then
			return x
		else
			return value(binding, env)
		end
	else
		return x
	end
end

print("hello world")
