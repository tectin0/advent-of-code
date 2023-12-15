with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Containers.Indefinite_Vectors;
with Ada.Assertions; use Ada.Assertions;

procedure Day7_Ada is
   procedure part_one(filename : in String; winnings: out Integer) is

      package StringVector is new Indefinite_Vectors (Index_Type => Positive, Element_Type => String);
      package IntegerVector is new Indefinite_Vectors (Index_Type => Positive, Element_Type => Integer);   

      package IVSorter is new IntegerVector.Generic_Sorting;

      file : Ada.Text_IO.File_Type;
      cards : StringVector.Vector;
      bids : IntegerVector.Vector;

      card_values : IntegerVector.Vector;
      sorted_card_values : IntegerVector.Vector;
      sorted_indices : IntegerVector.Vector;
   begin
      Ada.Text_IO.Put_Line ("Hello, world!");

      Ada.Text_IO.Open (File => file, Mode => Ada.Text_IO.In_File, Name => filename);

      while not Ada.Text_IO.End_Of_File (File => file) loop
         declare
            Line : constant String := Ada.Text_IO.Get_Line (File => file);
            --  string length 5
            Card : String (1..5);
            Bid : Integer := 0;
         begin
            -- iterate over the line and split it into card and bid at whitespace
            for i in Line'Range loop
               if Line (i) = ' ' then
                  Card := Line (1..i-1);
                  Bid := Integer'Value (Line(i+1..Line'Last));
                  exit;
               end if;
            end loop;

            StringVector.Append (cards, Card);
            IntegerVector.Append (bids, Bid);

         end;
      end loop;

      Ada.Text_IO.Close (File => file);

      for i in 1..cards.Length loop
         declare 
            -- i has to be Standard.Integer but is Ada.Containers.Count_Type
            Card : String := cards (Standard.Integer(i));
            Bid : Integer := bids (Standard.Integer(i));

            Value : Integer := 0;
            Is_Pair : Boolean := False;
            Is_Two_Pair : Boolean := False;
            Is_Three_Of_A_Kind : Boolean := False;
            Is_Full_House : Boolean := False;
            Is_Four_Of_A_Kind : Boolean := False;
            Is_Five_Of_A_Kind : Boolean := False;

            FirstLetter : Character := Card (1);
            SecondLetter : Character := Card (2);
            ThirdLetter : Character := Card (3);
            FourthLetter : Character := Card (4);
            FifthLetter : Character := Card (5);

            FirstLetterCount : Integer := 0;
            SecondLetterCount : Integer := 0;
            ThirdLetterCount : Integer := 0;
            FourthLetterCount : Integer := 0;
            FifthLetterCount : Integer := 0;
         begin
            Ada.Text_IO.Put_Line ("Card: " & Card & " Bid: " & Bid'Image);

            -- One Pair: 100000 Points
            -- Two Pair: 200000 Points
            -- Three of a Kind: 300000 Points
            -- Full House: 400000 Points
            -- Four of a Kind: 500000 Points
            -- Five of a Kind: 600000 Points

            for j in 1..Card'Length loop
               declare
                  Letter : Character := Card (j);
                  LetterValue : Integer := 0;
               begin

                  if Letter = 'A' then
                     Value := Value + (15 ** (5 - j)) * 14;
                  elsif Letter = 'K' then
                     Value := Value + (15 ** (5 - j)) * 13;
                  elsif Letter = 'Q' then
                     Value := Value + (15 ** (5 - j)) * 12;
                  elsif Letter = 'J' then
                     Value := Value + (15 ** (5 - j)) * 11;
                  elsif Letter = 'T' then
                     Value := Value + (15 ** (5 - j)) * 10;
                  else
                     Value := Value + (15 ** (5 - j)) * Integer'Value (( 1 => Letter));
                  end if;

                  if Letter = FirstLetter then
                     FirstLetterCount := FirstLetterCount + 1;
                  elsif Letter = SecondLetter then
                     SecondLetterCount := SecondLetterCount + 1;
                  elsif Letter = ThirdLetter then
                     ThirdLetterCount := ThirdLetterCount + 1;
                  elsif Letter = FourthLetter then
                     FourthLetterCount := FourthLetterCount + 1;
                  elsif Letter = FifthLetter then
                     FifthLetterCount := FifthLetterCount + 1;
                  end if;
               end;
            end loop;

            Ada.Text_IO.Put_Line ("FirstLetterCount: " & FirstLetterCount'Image);
            Ada.Text_IO.Put_Line ("SecondLetterCount: " & SecondLetterCount'Image);
            Ada.Text_IO.Put_Line ("ThirdLetterCount: " & ThirdLetterCount'Image);
            Ada.Text_IO.Put_Line ("FourthLetterCount: " & FourthLetterCount'Image);
            Ada.Text_IO.Put_Line ("FifthLetterCount: " & FifthLetterCount'Image);

            if FirstLetterCount = 2 or SecondLetterCount = 2 or ThirdLetterCount = 2 or FourthLetterCount = 2 or FifthLetterCount = 2 then
               Is_Pair := True;
            end if;

            if (FirstLetterCount = 2 and SecondLetterCount = 2) or (FirstLetterCount = 2 and ThirdLetterCount = 2) or (FirstLetterCount = 2 and FourthLetterCount = 2) or (FirstLetterCount = 2 and FifthLetterCount = 2) or (SecondLetterCount = 2 and ThirdLetterCount = 2) or (SecondLetterCount = 2 and FourthLetterCount = 2) or (SecondLetterCount = 2 and FifthLetterCount = 2) or (ThirdLetterCount = 2 and FourthLetterCount = 2) or (ThirdLetterCount = 2 and FifthLetterCount = 2) or (FourthLetterCount = 2 and FifthLetterCount = 2) then
               Is_Two_Pair := True;
            end if;

            if FirstLetterCount = 3 or SecondLetterCount = 3 or ThirdLetterCount = 3 or FourthLetterCount = 3 or FifthLetterCount = 3 then
               Is_Three_Of_A_Kind := True;
            end if;

            if (FirstLetterCount = 3 and SecondLetterCount = 2) or (FirstLetterCount = 3 and ThirdLetterCount = 2) or (FirstLetterCount = 3 and FourthLetterCount = 2) or (FirstLetterCount = 3 and FifthLetterCount = 2) or (SecondLetterCount = 3 and FirstLetterCount = 2) or (SecondLetterCount = 3 and ThirdLetterCount = 2) or (SecondLetterCount = 3 and FourthLetterCount = 2) or (SecondLetterCount = 3 and FifthLetterCount = 2) or (ThirdLetterCount = 3 and FirstLetterCount = 2) or (ThirdLetterCount = 3 and SecondLetterCount = 2) or (ThirdLetterCount = 3 and FourthLetterCount = 2) or (ThirdLetterCount = 3 and FifthLetterCount = 2) or (FourthLetterCount = 3 and FirstLetterCount = 2) or (FourthLetterCount = 3 and SecondLetterCount = 2) or (FourthLetterCount = 3 and ThirdLetterCount = 2) or (FourthLetterCount = 3 and FifthLetterCount = 2) or (FifthLetterCount = 3 and FirstLetterCount = 2) or (FifthLetterCount = 3 and SecondLetterCount = 2) or (FifthLetterCount = 3 and ThirdLetterCount = 2) or (FifthLetterCount = 3 and FourthLetterCount = 2) then
               Is_Full_House := True;
            end if;

            if FirstLetterCount = 4 or SecondLetterCount = 4 or ThirdLetterCount = 4 or FourthLetterCount = 4 or FifthLetterCount = 4 then
               Is_Four_Of_A_Kind := True;
            end if;

            if FirstLetterCount = 5 or SecondLetterCount = 5 or ThirdLetterCount = 5 or FourthLetterCount = 5 or FifthLetterCount = 5 then
               Is_Five_Of_A_Kind := True;
            end if;

            if Is_Five_Of_A_Kind then
               Ada.Text_IO.Put_Line ("Card " & Card & " is a Five of a Kind");
               Value := Value + 6000000;
            elsif Is_Four_Of_A_Kind then
               Ada.Text_IO.Put_Line ("Card " & Card & " is a Four of a Kind");
               Value := Value + 5000000;
            elsif Is_Full_House then
               Ada.Text_IO.Put_Line ("Card " & Card & " is a Full House");
               Value := Value + 4000000;  
            elsif Is_Three_Of_A_Kind then
               Ada.Text_IO.Put_Line ("Card" & Card & " is a Three of a Kind");
               Value := Value + 3000000;
            elsif Is_Two_Pair then
               Ada.Text_IO.Put_Line ("Card " & Card & " is a Two Pair");
               Value := Value + 2000000;
            elsif Is_Pair then
               Ada.Text_IO.Put_Line ("Card " & Card & " is a Pair");
               Value := Value + 1000000;
            end if;

            Ada.Text_IO.Put_Line ("Value: " & Value'Image);

            IntegerVector.Append (card_values, Value);

         end;
      end loop;

      -- I really don't know how to get sorted indices in Ada and I won't code a sorting algorithm 
      sorted_card_values := card_values;
      IVSorter.Sort (sorted_card_values);

      for i in 1..sorted_card_values.Length loop
         declare
            Value : Integer := sorted_card_values (Standard.Integer(i));
            Index : Integer := 0;
         begin
            for j in 1..card_values.Length loop
               if card_values (Standard.Integer(j)) = Value then
                  Index := (Standard.Integer(j));
                  exit;
               end if;
            end loop;

            IntegerVector.Append (sorted_indices, Index);
         end;
      end loop;

      declare
         Sum : Integer := 0;
      begin
         for i in 1..sorted_indices.Length loop
            declare 
               Index : Integer := sorted_indices (Standard.Integer(i));
               Bid : Integer := bids (Index);
               Card : String := cards (Index);
               Value : Integer := card_values (Index);
            begin
               Ada.Text_IO.Put_Line ("Card: " & Card & " Bid: " & Bid'Image & " Value: " & Value'Image & " Index: " & Index'Image);
               Sum := Sum + Integer(i) * Bid;
            end;
         end loop;

         -- I want out asap

         winnings := Sum;
      end;
   end part_one;

   procedure part_two(filename : in String; winnings: out Integer) is

      package StringVector is new Indefinite_Vectors (Index_Type => Positive, Element_Type => String);
      package IntegerVector is new Indefinite_Vectors (Index_Type => Positive, Element_Type => Integer);   

      package IVSorter is new IntegerVector.Generic_Sorting;

      file : Ada.Text_IO.File_Type;
      cards : StringVector.Vector;
      bids : IntegerVector.Vector;

      card_values : IntegerVector.Vector;
      sorted_card_values : IntegerVector.Vector;
      sorted_indices : IntegerVector.Vector;
   begin
      Ada.Text_IO.Put_Line ("Hello, world!");

      Ada.Text_IO.Open (File => file, Mode => Ada.Text_IO.In_File, Name => filename);

      while not Ada.Text_IO.End_Of_File (File => file) loop
         declare
            Line : constant String := Ada.Text_IO.Get_Line (File => file);
            --  string length 5
            Card : String (1..5);
            Bid : Integer := 0;
         begin
            -- iterate over the line and split it into card and bid at whitespace
            for i in Line'Range loop
               if Line (i) = ' ' then
                  Card := Line (1..i-1);
                  Bid := Integer'Value (Line(i+1..Line'Last));
                  exit;
               end if;
            end loop;

            StringVector.Append (cards, Card);
            IntegerVector.Append (bids, Bid);

         end;
      end loop;

      Ada.Text_IO.Close (File => file);

      for i in 1..cards.Length loop
         declare 
            -- i has to be Standard.Integer but is Ada.Containers.Count_Type
            Card : String := cards (Standard.Integer(i));
            Bid : Integer := bids (Standard.Integer(i));

            Value : Integer := 0;
            Is_Pair : Boolean := False;
            Is_Two_Pair : Boolean := False;
            Is_Three_Of_A_Kind : Boolean := False;
            Is_Full_House : Boolean := False;
            Is_Four_Of_A_Kind : Boolean := False;
            Is_Five_Of_A_Kind : Boolean := False;

            Pair_Count : Integer := 0;

            FirstLetter : Character := Card (1);
            SecondLetter : Character := Card (2);
            ThirdLetter : Character := Card (3);
            FourthLetter : Character := Card (4);
            FifthLetter : Character := Card (5);

            FirstLetterCount : Integer := 0;
            SecondLetterCount : Integer := 0;
            ThirdLetterCount : Integer := 0;
            FourthLetterCount : Integer := 0;
            FifthLetterCount : Integer := 0;

            Counts : IntegerVector.Vector;

            NonJokerCounts : IntegerVector.Vector;

            FirstLetterIsJoker : Boolean := False;
            SecondLetterIsJoker : Boolean := False;
            ThirdLetterIsJoker : Boolean := False;
            FourthLetterIsJoker : Boolean := False;
            FifthLetterIsJoker : Boolean := False;

            JokerCount : Integer := 0;
         begin
            Ada.Text_IO.Put_Line ("Card: " & Card & " Bid: " & Bid'Image);

            -- One Pair: 100000 Points
            -- Two Pair: 200000 Points
            -- Three of a Kind: 300000 Points
            -- Full House: 400000 Points
            -- Four of a Kind: 500000 Points
            -- Five of a Kind: 600000 Points

            for j in 1..Card'Length loop
               declare
                  Letter : Character := Card (j);
                  LetterValue : Integer := 0;
               begin
                  if Letter = 'J' then
                     JokerCount := JokerCount + 1;

                     if j = 1 then
                        FirstLetterIsJoker := True;
                     elsif j = 2 then
                        SecondLetterIsJoker := True;
                     elsif j = 3 then
                        ThirdLetterIsJoker := True;
                     elsif j = 4 then
                        FourthLetterIsJoker := True;
                     elsif j = 5 then
                        FifthLetterIsJoker := True;
                     end if;
                  end if;

                  if Letter = 'A' then
                     Value := Value + (15 ** (5 - j)) * 14;
                  elsif Letter = 'K' then
                     Value := Value + (15 ** (5 - j)) * 13;
                  elsif Letter = 'Q' then
                     Value := Value + (15 ** (5 - j)) * 12;
                  elsif Letter = 'J' then
                     Value := Value + (15 ** (5 - j)) * 1; -- Joker not worth anything
                  elsif Letter = 'T' then
                     Value := Value + (15 ** (5 - j)) * 10;
                  else
                     Value := Value + (15 ** (5 - j)) * Integer'Value (( 1 => Letter));
                  end if;

                  if Letter = FirstLetter then
                     FirstLetterCount := FirstLetterCount + 1;
                  elsif Letter = SecondLetter then
                     SecondLetterCount := SecondLetterCount + 1;
                  elsif Letter = ThirdLetter then
                     ThirdLetterCount := ThirdLetterCount + 1;
                  elsif Letter = FourthLetter then
                     FourthLetterCount := FourthLetterCount + 1;
                  elsif Letter = FifthLetter then
                     FifthLetterCount := FifthLetterCount + 1;
                  end if;
               end;
            end loop;

            Ada.Text_IO.Put_Line ("FirstLetterCount: " & FirstLetterCount'Image);
            Ada.Text_IO.Put_Line ("SecondLetterCount: " & SecondLetterCount'Image);
            Ada.Text_IO.Put_Line ("ThirdLetterCount: " & ThirdLetterCount'Image);
            Ada.Text_IO.Put_Line ("FourthLetterCount: " & FourthLetterCount'Image);
            Ada.Text_IO.Put_Line ("FifthLetterCount: " & FifthLetterCount'Image);

            Counts.Append (FirstLetterCount);
            Counts.Append (SecondLetterCount);
            Counts.Append (ThirdLetterCount);
            Counts.Append (FourthLetterCount);
            Counts.Append (FifthLetterCount);

            if FirstLetterIsJoker = False then
               NonJokerCounts.Append (FirstLetterCount);
            end if;

            if SecondLetterIsJoker = False then
               NonJokerCounts.Append (SecondLetterCount);
            end if;

            if ThirdLetterIsJoker = False then
               NonJokerCounts.Append (ThirdLetterCount);
            end if;

            if FourthLetterIsJoker = False then
               NonJokerCounts.Append (FourthLetterCount);
            end if;

            if FifthLetterIsJoker = False then
               NonJokerCounts.Append (FifthLetterCount);
            end if;

            for j in 1..NonJokerCounts.Length loop
               declare
                  Count : Integer := NonJokerCounts (Standard.Integer(j));
               begin
                  if Count = 2 then
                     Pair_Count := Pair_Count + 1;
                     Is_Pair := True;
                  elsif Count = 3 then
                     Is_Three_Of_A_Kind := True;
                  elsif Count = 4 then
                     Is_Four_Of_A_Kind := True;
                  elsif Count = 5 then
                     Is_Five_Of_A_Kind := True;
                  end if;
               end;
            end loop;

            -- masterfully crafted code incoming

            if Is_Five_Of_A_Kind then
               Ada.Text_IO.Put_Line ("Card " & Card & " is a Five of a Kind");
               Value := Value + 6000000;
            elsif Is_Four_Of_A_Kind and (JokerCount = 1) then
               Ada.Text_IO.Put_Line ("Card " & Card & " is a Five of a Kind");
               Value := Value + 6000000;
            elsif Is_Three_Of_A_Kind and (JokerCount = 2) then
               Ada.Text_IO.Put_Line ("Card " & Card & " is a Five of a Kind");
               Value := Value + 6000000;
            elsif Is_Pair and (JokerCount = 3) then
               Ada.Text_IO.Put_Line ("Card " & Card & " is a Five of a Kind");
               Value := Value + 6000000;
            elsif JokerCount = 4 then
               Ada.Text_IO.Put_Line ("Card " & Card & " is a Five of a Kind");
               Value := Value + 6000000;
            elsif JokerCount = 5 then
               Ada.Text_IO.Put_Line ("Card " & Card & " is a Five of a Kind");
               Value := Value + 6000000;

            elsif Is_Four_Of_A_Kind then
               Ada.Text_IO.Put_Line ("Card " & Card & " is a Four of a Kind");
               Value := Value + 5000000;
            elsif Is_Three_Of_A_Kind and (JokerCount = 1) then
               Ada.Text_IO.Put_Line ("Card " & Card & " is a Four of a Kind");
               Value := Value + 5000000;
            elsif Is_Pair and (JokerCount = 2) then
               Ada.Text_IO.Put_Line ("Card " & Card & " is a Four of a Kind");
               Value := Value + 5000000;
            elsif JokerCount = 3 then
               Ada.Text_IO.Put_Line ("Card " & Card & " is a Four of a Kind");
               Value := Value + 5000000;

            elsif Is_Pair and Is_Three_Of_A_Kind then
               Ada.Text_IO.Put_Line ("Card " & Card & " is a Full House");
               Value := Value + 4000000;
            elsif Is_Pair and (Pair_Count = 2) and (JokerCount = 1) then
               Ada.Text_IO.Put_Line ("Card " & Card & " is a Full House");
               Value := Value + 4000000;
            
            elsif Is_Three_Of_A_Kind then
               Ada.Text_IO.Put_Line ("Card" & Card & " is a Three of a Kind");
               Value := Value + 3000000;
            elsif Is_Pair and (Pair_Count = 1) and (JokerCount = 1) then
               Ada.Text_IO.Put_Line ("Card" & Card & " is a Three of a Kind");
               Value := Value + 3000000;
            elsif JokerCount = 2 then
               Ada.Text_IO.Put_Line ("Card" & Card & " is a Three of a Kind");
               Value := Value + 3000000;
            
            elsif Is_Pair and (Pair_Count = 2) then
               Ada.Text_IO.Put_Line ("Card " & Card & " is a Two Pair");
               Value := Value + 2000000;
            elsif JokerCount = 2 then
               Ada.Text_IO.Put_Line ("Card " & Card & " is a Two Pair");
               Value := Value + 2000000;
            
            elsif Is_Pair then
               Ada.Text_IO.Put_Line ("Card " & Card & " is a Pair");
               Value := Value + 1000000;
            elsif JokerCount = 1 then  
               Ada.Text_IO.Put_Line ("Card " & Card & " is a Pair");
               Value := Value + 1000000;
            end if; 

            Ada.Text_IO.Put_Line ("Value: " & Value'Image);

            IntegerVector.Append (card_values, Value);

         end;
      end loop;

      -- I really don't know how to get sorted indices in Ada and I won't code a sorting algorithm 
      sorted_card_values := card_values;
      IVSorter.Sort (sorted_card_values);

      for i in 1..sorted_card_values.Length loop
         declare
            Value : Integer := sorted_card_values (Standard.Integer(i));
            Index : Integer := 0;
         begin
            for j in 1..card_values.Length loop
               if card_values (Standard.Integer(j)) = Value then
                  Index := (Standard.Integer(j));
                  exit;
               end if;
            end loop;

            IntegerVector.Append (sorted_indices, Index);
         end;
      end loop;

      declare
         Sum : Integer := 0;
      begin
         for i in 1..sorted_indices.Length loop
            declare 
               Index : Integer := sorted_indices (Standard.Integer(i));
               Bid : Integer := bids (Index);
               Card : String := cards (Index);
               Value : Integer := card_values (Index);
            begin
               Ada.Text_IO.Put_Line ("Card: " & Card & " Bid: " & Bid'Image & " Value: " & Value'Image & " Index: " & Index'Image);
               Sum := Sum + Integer(i) * Bid;
            end;
         end loop;

         -- I want out asap

         winnings := Sum;
      end;
   end part_two;

   example_answer_part_one : Integer := 0;
   example_answer_part_two : Integer := 0;

   input_answer_part_one : Integer := 0;
   input_answer_part_two : Integer := 0;

begin
   part_one ("example", example_answer_part_one);
   
   Assert (example_answer_part_one = 6440, "Example answer is wrong");

   part_one ("input", input_answer_part_one);


   part_two ("example", example_answer_part_two);

   Assert (example_answer_part_two = 5905, "Example answer is wrong");

   part_two ("input", input_answer_part_two);

   Ada.Text_IO.Put_Line ("Example 1 Answer: " & example_answer_part_one'Image);
   Ada.Text_IO.Put_Line ("Example 2 Answer: " & example_answer_part_two'Image);
   Ada.Text_IO.Put_Line ("Part 1 Answer: " & input_answer_part_one'Image);
   Ada.Text_IO.Put_Line ("Part 2 Answer: " & input_answer_part_two'Image);
end Day7_Ada;

