local function sum_of_numbers_adjacents_to_symbols(filename)
    local file = io.open(filename, "r")

    if not file then
        print("File not found!")
        os.exit()
    end

    -- 2d grid
    local grid = {}

    local line_count = 0

    for line in file:lines() do
        line_count = line_count + 1

        grid[line_count] = {}

        for i = 1, #line do
            grid[line_count][i] = line:sub(i, i)
        end
    end

    file:close()

    local grid_width = #grid[1]
    local grid_height = #grid

    local kernel = {
        {0, -1}, -- up
        {1, -1}, -- up right
        {1, 0}, -- right
        {1, 1}, -- down right
        {0, 1}, -- down
        {-1, 1}, -- down left
        {-1, 0}, -- left
        {-1, -1} -- up left
    }

    -- list of coordinates of cells in the grid that contain a number and are next to a symbol
    local digits_next_to_symbol = {}

    for i = 1, #grid do
        for j = 1, #grid[i] do
            local cell =  grid[i][j]
            -- is cell a number
            if tonumber(cell) then
                -- check if any of the cells around it are a symbol
                for k = 1, #kernel do
                    local x = j + kernel[k][1]
                    local y = i + kernel[k][2]

                    if x > 0 and x <= grid_width and y > 0 and y <= grid_height then
                        local check_cell = grid[y][x]

                        -- check if cell is anything except a number or a dot
                        if not tonumber(check_cell) and check_cell ~= "." then
                            table.insert(digits_next_to_symbol, {j, i})
                        end
                    end
                end
            end
        end
    end

    local numbers = {}

    local last_x = nil
    local last_y = nil

    for i = 1, #digits_next_to_symbol do
        local x = digits_next_to_symbol[i][1]
        local y = digits_next_to_symbol[i][2]

        local skip = false

        -- check if this coordinate is already part of a number
        if last_x and last_y then
            if x == last_x + 1 and y == last_y then
                skip = true
            end
        end

        last_x = x
        last_y = y

        if not skip then
            local complete_number = ""
            -- check to left until we hit not a number
            for j = x, 1, -1 do
                local cell = grid[y][j]

                if tonumber(cell) then
                    complete_number = cell .. complete_number
                else
                    break
                end
            end

            -- check to right until we hit not a number
            for j = x + 1, grid_width do
                local cell = grid[y][j]

                if tonumber(cell) then
                    complete_number = complete_number .. cell
                else
                    break
                end
            end

            table.insert(numbers, complete_number)
        end
    end

    local sum = 0

    for i = 1, #numbers do
        sum = sum + tonumber(numbers[i])
    end

    return sum
end

local sum_example = sum_of_numbers_adjacents_to_symbols("example")

assert(sum_example == 4361, "Sum is not 4361, it is " .. sum_example)

local sum_input = sum_of_numbers_adjacents_to_symbols("input")

print("Part 1 Answer: " .. sum_input)