with Interfaces.C;

package Mmap is
   type Mmap_File is limited private;

   type Mmap_Record is record
      Start_Idx : Interfaces.C.long_long;
      End_Idx   : Interfaces.C.long_long;
      Value     : Interfaces.C.unsigned_char;
   end record;
   pragma Convention (C, Mmap_Record);

   procedure Open (Filename : String);
   procedure Close;
   function Length return Long_Integer;
   function Get (Index : Long_Integer) return Mmap_Record;

private
   type Mmap_File is limited null record;
   pragma Convention (C, Mmap_File);
end Mmap;
