$lines = Get-Content -Path input

$grid = New-Object 'object[,]' $lines.Length, $lines[0].Length

for ($i = 0; $i -lt $lines.Length; $i++) {
    for ($j = 0; $j -lt $lines[$i].Length; $j++) {
        $grid[$i, $j] = $lines[$i][$j]
    }
}

$columns_without_galaxy = @()

for ($x = 0; $x -lt $grid.GetLength(1); $x++) {
    $number_galaxies = 0

    for ($y = 0; $y -lt $grid.GetLength(0); $y++) {
        if ($grid[$y, $x] -eq '#') {
            $number_galaxies++
        }
    }

    if ($number_galaxies -eq 0) {
        $columns_without_galaxy += $x
    }
}

$rows_without_galaxy = @()

for ($y = 0; $y -lt $grid.GetLength(0); $y++) {
    $number_galaxies = 0

    for ($x = 0; $x -lt $grid.GetLength(1); $x++) {
        if ($grid[$y, $x] -eq '#') {
            $number_galaxies++
        }
    }

    if ($number_galaxies -eq 0) {
        $rows_without_galaxy += $y
    }
}

$new_row_indices = @()

for ($y = 0; $y -lt $grid.GetLength(0); $y++) {
    $new_row_indices += $y
    if ($rows_without_galaxy -contains $y) {
        $new_row_indices += $y
    }    
}

$new_columns_indices = @()

for ($x = 0; $x -lt $grid.GetLength(1); $x++) {
    $new_columns_indices += $x
    if ($columns_without_galaxy -contains $x) {
        $new_columns_indices += $x
    }    
}

$new_grid = New-Object 'object[,]' $new_row_indices.Length, $new_columns_indices.Length

for ($y = 0; $y -lt $new_row_indices.Length; $y++) {
    for ($x = 0; $x -lt $new_columns_indices.Length; $x++) {
        $new_grid[$y, $x] = $grid[$new_row_indices[$y], $new_columns_indices[$x]]
    }
}

$galaxy_positions = @()

for ($y = 0; $y -lt $new_grid.GetLength(0); $y++) {
    for ($x = 0; $x -lt $new_grid.GetLength(1); $x++) {
        if ($new_grid[$y, $x] -eq '#') {
            # ,@ is the array concatenation operator
            $galaxy_positions += ,@($x, $y)
        }
    }
}

$galaxy_count = $galaxy_positions.Length

$galaxy_pair_count = $galaxy_count * ($galaxy_count - 1) / 2

$pairs_of_galaxies = New-Object 'object[,]' $galaxy_pair_count, 2
$pair_galaxy_indices = New-Object 'object[,]' $galaxy_pair_count, 2

$pair_index = 0

for ($i = 0; $i -lt $galaxy_positions.Length; $i++) {
    for ($j = $i + 1; $j -lt $galaxy_positions.Length; $j++) {
        $pairs_of_galaxies[$pair_index, 0] = $galaxy_positions[$i]
        $pairs_of_galaxies[$pair_index, 1] = $galaxy_positions[$j]

        $pair_galaxy_indices[$pair_index, 0] = $i
        $pair_galaxy_indices[$pair_index, 1] = $j

        $pair_index++
    }
}


if ($pair_index -ne $galaxy_pair_count) {
    Write-Host "Error: incorrect number of pairs of galaxies"
    exit
}

$shortest_path_lengths = New-Object 'object[,]' $galaxy_pair_count, 1

for ($i = 0; $i -lt $pairs_of_galaxies.GetLength(0); $i++) {
    $x1 = $pairs_of_galaxies[$i, 0][0]
    $y1 = $pairs_of_galaxies[$i, 0][1]
    $x2 = $pairs_of_galaxies[$i, 1][0]
    $y2 = $pairs_of_galaxies[$i, 1][1]

    $shortest_path_lengths[$i, 0] = [Math]::Abs($x1 - $x2) + [Math]::Abs($y1 - $y2)
}

$sum_of_shortest_path_lengths = 0

for ($i = 0; $i -lt $shortest_path_lengths.GetLength(0); $i++) {
    $length = $shortest_path_lengths[$i, 0]
    $sum_of_shortest_path_lengths += $length
}

Write-Host "Part 1 Answer: $sum_of_shortest_path_lengths"