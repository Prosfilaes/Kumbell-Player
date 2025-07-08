with Interfaces.C;
with Interfaces.C.Strings;
with System;

package body Mmap is

   use Interfaces;
   use Interfaces.C;
   use C.Strings;

   -- C function imports
   function mmap_open (filename : chars_ptr) return C.int;
   pragma Import (C, mmap_open, "mmap_open");

   procedure mmap_close;
   pragma Import (C, mmap_close, "mmap_close");

   function mmap_length return C.size_t;
   pragma Import (C, mmap_length, "mmap_length");

   function mmap_get (index : C.size_t) return Mmap_Record;
   pragma Import (C, mmap_get, "mmap_get");

   procedure Open (Filename : String) is
      C_Filename : chars_ptr := New_String (Filename);
      Err : c.int := mmap_open (C_Filename);
      Mmap_Error : exception;
   begin
      Free (C_Filename);
      if Err /= 0 then
          raise Mmap_Error with Err'Image;
      end if;
   end Open;

   procedure Close  is
   begin
      mmap_close;
   end Close;

   function Length return Long_Integer is
   begin
      return Long_Integer (mmap_length);
   end Length;

   function Get (Index : Long_Integer) return Mmap_Record is
   begin
      return mmap_get (C.size_t (Index));
   end Get;

end Mmap;
