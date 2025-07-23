
function Format_Number (N : Long_Integer) return String is
   -- Helper to insert commas
   function Comma_Separated (Num : Long_Integer) return String is
      S : constant String := Num'Image;
      Str : constant String := S (S'First + 1 .. S'Last); -- Remove leading space
      Result : String (1 .. Str'Length + (Str'Length - 1) / 3);
      Pos : Integer := Str'Length;
      Res_Pos : Integer := Result'Length;
      Count : Integer := 0;
   begin
      if Num < 1000 then
         return Str;
      end if;
      while Pos >= 1 loop
         Result (Res_Pos) := Str (Pos);
         Count := Count + 1;
         Res_Pos := Res_Pos - 1;
         if Count = 3 and Pos > 1 then
            Result (Res_Pos) := ',';
            Res_Pos := Res_Pos - 1;
            Count := 0;
         end if;
         Pos := Pos - 1;
      end loop;
      return Result (Res_Pos + 1 .. Result'Length);
   end Comma_Separated;

   Billion         : constant Long_Integer := 1_000_000_000;
   Million         : constant Long_Integer := 1_000_000;
   Thousand        : constant Long_Integer := 1_000;
   Hundred_Million : constant Long_Integer := 100_000_000;
   Hundred_Thousand: constant Long_Integer := 100_000;

   -- Helper to format x.x style using integer math
   function Format_X_X (N, Divisor : Long_Integer; Suffix : String) return String is
      Whole : constant Integer := Integer (N / Divisor);
      Fraction : constant Integer := Integer ((N mod Divisor) * 10 / Divisor);
   begin
      return Integer'Image (Whole) & "." & Integer'Image (Fraction)(2 .. 2) & " " & Suffix;
   end Format_X_X;

begin
   if N < 1000 or else N mod 10 /= 0 then
      return N'Image;
   elsif N mod Billion = 0 and N / Billion < 1_000 then
      return Integer'Image (Integer (N / Billion)) & " billion";
   elsif N mod Million = 0 and N / Million < 1_000 then
      return Integer'Image (Integer (N / Million)) & " million";
   elsif N mod Thousand = 0 and N / Thousand < 1_000 then
      return Integer'Image (Integer (N / Thousand)) & " thousand";
   elsif N mod Hundred_Million = 0 then
      return Format_X_X (N, Billion, "billion");
   elsif N mod Hundred_Thousand = 0 then
      return Format_X_X (N, Thousand, "thousand");
   else
      return N'Image;
   end if;
end Format_Number;
