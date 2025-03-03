local NUM_INPUTS = 3
local NUM_OUTPUTS = 2
local STARTING_DEPTH = 3
local POPULATION_SIZE = 10000
local TOURNAMENT_SIZE = 7
local MUTATION_RATE = 0.05
local MAX_GENERATIONS = 300
local BEST_POPULATION_SIZE = 1000
local MAX_TREE_SIZE = 100
local SIZE_DECREASING_FACTOR = 0.99
local SIZE_DECREASING_THRESHOLD = 0.2

local random = math.random

-- math.randomseed(os.time())

local primitives = {
    {arity = 2, func = function(a,b) return a + b end},
    {arity = 2, func = function(a,b) return a - b end},
    {arity = 2, func = function(a,b) return a * b end},
    {arity = 1, func = function(a) return math.sin(a) end},
    {arity = 1, func = function(a) return math.cos(a) end},
    {arity = 2, func = function(a,b) return a / (math.abs(b)+1e-9) end},

        -- Additions: More basic arithmetic and operations
    -- {arity = 2, func = function(a,b) return a ^ b end},
    {arity = 2, func = function(a,b) return a % b end},
    {arity = 2, func = function(a,b) return math.max(a, b) end}, 
    {arity = 2, func = function(a,b) return math.min(a, b) end},

    -- Trigonometric functions
    -- {arity = 1, func = function(a) return math.tan(a) end}, 
    -- {arity = 1, func = function(a) return math.asin(a) end},  
    -- {arity = 1, func = function(a) return math.acos(a) end},
    -- {arity = 1, func = function(a) return math.atan(a) end},
    -- {arity = 2, func = function(a,b) return math.atan2(a, b) end}, 

    -- Logarithmic and Exponential Functions
    -- {arity = 1, func = function(a) return math.log(math.abs(a)) end}, 
    -- {arity = 2, func = function(a,b) return math.log(math.abs(a), math.abs(b)) end},
    -- {arity = 1, func = function(a) return math.exp(a) end}, 
    -- {arity = 2, func = function(a,b) return math.pow(a, b) end},

    -- Square Root and Absolute Value
    {arity = 1, func = function(a)
        if a < 0 then return -math.sqrt(math.abs(a)) end
        return math.sqrt(math.abs(a)) 
    end},
    {arity = 1, func = function(a) return math.abs(a) end},

    -- Hyperbolic functions
    {arity = 1, func = function(a) return math.sinh(a) end},
    {arity = 1, func = function(a) return math.cosh(a) end},
    {arity = 1, func = function(a) return math.tanh(a) end},

    -- Floor, Ceiling, and Fractional Part
    {arity = 1, func = function(a) return math.floor(a) end},
    {arity = 1, func = function(a) return math.ceil(a) end},
    {arity = 1, func = function(a) return a - math.floor(a) end},

    -- Statistical functions
    -- {arity = 1, func = function(a) return a * a end},
}

local function create_node(value, children)
    return {
        value = value,
        children = children
    }
end

local function clone_node(node)
    local children = {}
    if node.children ~= nil then
        for i, child in ipairs(node.children) do
            table.insert(children, clone_node(child))
        end
    end
    return create_node(node.value, children)
end

local function evaluate_node(node, inputs)
    if type(node.value) == "function" then
        local args = {}
        for _, child in ipairs(node.children) do
            table.insert(args, evaluate_node(child, inputs))
        end
        return node.value(unpack(args))
    end
    if string.sub(node.value, 1, 1) == "x" then
        local index = tonumber(string.sub(node.value, 2))
        return inputs[index] or 0
    end
    return tonumber(node.value)
end

local function generate_tree(max_depth)
    if max_depth == 0  then
        if random() < 0.5 then
            return create_node("x" .. tostring(random(NUM_INPUTS)))
        else
            return create_node(tostring(random() * 4 -2))
        end
    else
        local primitive = primitives[random(#primitives)]
        local children = {}
        for i = 1, primitive.arity do
            children[i] = generate_tree(max_depth - 1)
        end
        return create_node(primitive.func, children)
    end
end

local function generate_individuals()
    local individuals = {}
    for i = 1, NUM_OUTPUTS do
        table.insert(individuals, generate_tree(STARTING_DEPTH))
    end
    return individuals
end

local function calculate_size(individual)
    local size = 0
    local function traverse(node)
        size = size + 1
        if node.children ~= nil then
            for i, child in ipairs(node.children) do
                traverse(child)
            end
        end
    end
    traverse(individual)
    return size
end

local function fitness(individual, data)
    local total_errors = {}
    local sizes = {}
    for i = 1, NUM_OUTPUTS do
        sizes[i] = calculate_size(individual[i])
        total_errors[i] = 0
    end
    for i, point in ipairs(data) do
        local outputs = {}
        for i, tree in ipairs(individual) do
            outputs[i] = evaluate_node(tree, point.inputs)
        end

        -- calculate error
        for i, target in ipairs(point.outputs) do
            local err = outputs[i] - target
            -- total_errors[i] = total_errors[i] + err * err
            total_errors[i] = total_errors[i] + math.abs(err) 
        end

    end
    -- size penalty
    for i, tree in ipairs(individual) do
        if sizes[i] > MAX_TREE_SIZE then
            total_errors[i] = total_errors[i] * ((sizes[i] / MAX_TREE_SIZE) ^ 2)
        end
    end
    local total_error = 0
    local total_size = 0
    for i = 1, NUM_OUTPUTS do
        total_error = total_error + total_errors[i]
        total_size = total_size + sizes[i]
    end
    return total_error / (#data * NUM_OUTPUTS), total_size / NUM_OUTPUTS
end

local function tournament_selection(population, fitnesses)
    local best
    for i = 1, TOURNAMENT_SIZE do
        local candidate = population[random(#population)]
        local score = fitnesses[candidate]

        if not best or score < fitnesses[best] then
            best = candidate
        end
    end
    return best
end

local function crossover(a, b)
    local new_a = {}
    local new_b = {}

    for i = 1, NUM_OUTPUTS do
        local a_tree = clone_node(a[i])
        local b_tree = clone_node(b[i])

        local function get_all_nodes(tree)
            local nodes = {}
            local function traverse(node)
                table.insert(nodes, node)
                for i, child in ipairs(node.children) do
                    traverse(child)
                end
            end
            traverse(tree)
            return nodes
        end

        local a_nodes = get_all_nodes(a_tree)
        local b_nodes = get_all_nodes(b_tree)

        if #a_nodes > 0 and #b_nodes > 0 then
            local a_node = a_nodes[random(#a_nodes)]
            local b_node = b_nodes[random(#b_nodes)]
            a_node.value, b_node.value = b_node.value, a_node.value
            a_node.children, b_node.children = b_node.children, a_node.children
        end

        table.insert(new_a, a_tree)
        table.insert(new_b, b_tree)
    end

    return new_a, new_b
end

local function mutate(individual)
    local mutated = {}

    for i, tree in ipairs(individual) do
        local new_tree = clone_node(tree)

        local function mutate_node(node)
            if random() < MUTATION_RATE then
                if type(node.value) == "function" then
                    local new_prim = primitives[random(#primitives)]
                    if new_prim.arity == #node.children then
                        node.value = new_prim.func
                    end
                else
                    if random() < 0.5 then
                        if random() < 0.7 then
                            node.value = "x" .. tostring(random(NUM_INPUTS))
                        else
                            node.value = tostring(random() * 4 - 2)
                        end
                    end
                end
            end
            for i, child in ipairs(node.children) do
                mutate_node(child)
            end
        end
        mutate_node(new_tree)
        table.insert(mutated, new_tree)
    end

    return mutated
end

local function print_individual(individual)
    local function node_to_string(node)
        if type(node.value) == "function" then
            local args = {}
            for _, child in ipairs(node.children) do
                table.insert(args, node_to_string(child))
            end
            local func_map = {
                [primitives[1].func] = "add",
                [primitives[2].func] = "sub",
                [primitives[3].func] = "mul",
                [primitives[4].func] = "sin",
                [primitives[5].func] = "cos",
                [primitives[6].func] = "div"
            }
            local symbol = func_map[node.value] or "?"
            return "(" .. table.concat(args, " " .. symbol .. " ") .. ")"
        else
            return node.value
        end
    end

    for i, tree in ipairs(individual) do
        print(("Output %d: %s"):format(i, node_to_string(tree)))
    end
end

local function regression(data)
    local population = {}
    for i = 1, POPULATION_SIZE do
        table.insert(population, generate_individuals())
    end

    for generations = 1, MAX_GENERATIONS do
        local fitnesses = {}
        local sizes = {}
        for i, individual in ipairs(population) do
            fitnesses[individual], sizes[i] = fitness(individual, data)
        end

        table.sort(population, function(a, b) return fitnesses[a] < fitnesses[b] end)

        local new_population = {}
        for i = 1, BEST_POPULATION_SIZE do
            table.insert(new_population, population[i])
        end

        while #new_population < POPULATION_SIZE do
            local parent1 = tournament_selection(population, fitnesses)
            local parent2 = tournament_selection(population, fitnesses)

            local child1, child2 = crossover(parent1, parent2)
            table.insert(new_population, mutate(child1))
            if #new_population < POPULATION_SIZE then
                table.insert(new_population, child2)
            end
        end

        population = new_population

        if fitnesses[population[1]] < SIZE_DECREASING_THRESHOLD then
            MAX_TREE_SIZE = math.floor(MAX_TREE_SIZE * SIZE_DECREASING_FACTOR)
        end

        local total_size = 0
        for i = 1, #sizes do
            total_size = total_size + sizes[i]
        end

        print("Generation " .. generations .. " minimal error: " .. string.format("%.8f", fitnesses[population[1]]) .. " max size: " .. MAX_TREE_SIZE .. " average size: " .. 
        string.format("%.2f", total_size/#sizes) .. " best size: " .. sizes[1])
    end

    return population[1]
end

local data = {}
for i = 1, 4 do
    local x1 = random() * 10 - 5
    local x2 = random() * 10 - 5
    local x3 = random() * 10 - 5
    
    table.insert(data, {
        inputs = {x1, x2, x3},
        outputs = {
            x1 * x2 + math.sin(x3),
            (x1 + x3) * 0.5 - x2
        }
    })
end

local best = regression(data)

-- local item = generate_individuals()
-- local item2 = generate_individuals()
-- print(fitness(item, data))
-- print(fitness(item2, data))
-- local child1, child2 = crossover(item, item2)
-- print(fitness(child1, data))
-- print(fitness(child2, data))
-- local individuals = mutate(item)
-- print_individual(item)
-- for i = 1, #item do
--     table.insert(individuals, mutate(clone_node(item[i])))
-- end
-- print(fitness(individuals, data))