program part_one_and_two;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils;

procedure ReadFileToStrings(const filename: string; var strings:System.TArray<System.string>);
var
  F: TextFile;
  filecontent: string;
begin
var AllText: string := '';
AssignFile(F, filename);
Reset(F);
while not Eof(F) do
begin
  ReadLn(F, filecontent);
  AllText := AllText + filecontent + '\n';
end;
AllText := AllText.Remove(AllText.Length - 2, 2);
CloseFile(F);
strings := AllText.Split(['\n']);
end;

const RED_MAX: integer = 12;
const GREEN_MAX: integer = 13;
const BLUE_MAX: integer = 14;

procedure SumOfValidGames(const filename: string; var sum_valid_games: integer);
begin
  var filecontent: System.TArray<System.string>;
  ReadFileToStrings(filename, filecontent);

  for var i := 0 to Length(filecontent) - 1 do
  begin
    var is_game_valid := true;

    var first_line_split := filecontent[i].Split([':']);
    var game_id := StrToInt(first_line_split[0].Split([' '])[1]);

    var game := first_line_split[1].Split([';']);
    for var j := 0 to Length(game) - 1 do
    begin
      var blue_count := 0;
      var red_count := 0;
      var green_count := 0;

      var round := game[j].Split([',']);
      for var k := 0 to Length(round) - 1 do
      begin
        var color := round[k].Split([' ']);
        if color[2] = 'blue' then
          blue_count := blue_count + StrToInt(color[1])
        else if color[2] = 'red' then
          red_count := red_count + StrToInt(color[1])
        else if color[2] = 'green' then
          green_count := green_count + StrToInt(color[1]);

        if (red_count > RED_MAX) or (green_count > GREEN_MAX) or (blue_count > BLUE_MAX) then
        begin
          is_game_valid := false;
          break;
        end;        
      end;
    end;

    if is_game_valid then
      sum_valid_games := sum_valid_games + game_id;

    // Writeln('Game ', game_id, ' is ', is_game_valid);
  end;
end;

procedure SumPowerMinimumCubeSet(const filename: string; var sum_power_minimum_cube_set: integer);
begin
  var filecontent: System.TArray<System.string>;
  ReadFileToStrings(filename, filecontent);

  for var i := 0 to Length(filecontent) - 1 do
  begin
    var power_minimum_cube_set := 0;
    var max_red_in_game := 0;
    var max_green_in_game := 0;
    var max_blue_in_game := 0;

    var first_line_split := filecontent[i].Split([':']);
    var game_id := StrToInt(first_line_split[0].Split([' '])[1]);

    var game := first_line_split[1].Split([';']);
    for var j := 0 to Length(game) - 1 do
    begin
      var blue_count := 0;
      var red_count := 0;
      var green_count := 0;

      var round := game[j].Split([',']);
      for var k := 0 to Length(round) - 1 do
      begin
        var color := round[k].Split([' ']);
        if color[2] = 'blue' then
          blue_count := blue_count + StrToInt(color[1])
        else if color[2] = 'red' then
          red_count := red_count + StrToInt(color[1])
        else if color[2] = 'green' then
          green_count := green_count + StrToInt(color[1]);

        if red_count > max_red_in_game then
          max_red_in_game := red_count;

        if green_count > max_green_in_game then
          max_green_in_game := green_count;    

        if blue_count > max_blue_in_game then
          max_blue_in_game := blue_count;
      end;
    end;

    power_minimum_cube_set := max_red_in_game * max_green_in_game * max_blue_in_game;

    sum_power_minimum_cube_set := sum_power_minimum_cube_set + power_minimum_cube_set;

    // Writeln('Game ', game_id, ' has power minimum cube set ', power_minimum_cube_set);
  end;
end;

procedure ExecuteProgram;
begin
  var sum_valid_games := 0;

  SumOfValidGames('example', sum_valid_games);
  assert (sum_valid_games = 8);

  sum_valid_games := 0;

  SumOfValidGames('input', sum_valid_games);
  Writeln('Part One Answer: ', sum_valid_games);

  var sum_power_minimum_cube_set := 0;

  SumPowerMinimumCubeSet('example', sum_power_minimum_cube_set);
  assert (sum_power_minimum_cube_set = 2286);

  sum_power_minimum_cube_set := 0;

  SumPowerMinimumCubeSet('input', sum_power_minimum_cube_set);
  Writeln('Part Two Answer: ', sum_power_minimum_cube_set);

end;

begin
  try
    ExecuteProgram;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.


