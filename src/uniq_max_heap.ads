with Ada.Containers.Vectors; use Ada.Containers;

generic
   type T is (<>);
package Uniq_Max_Heap is
pragma Preelaborate (Uniq_Max_Heap);
   type Max_Heap_Type is tagged private;
   function Size (heap: Max_Heap_Type) return Natural;
   function Max (heap: Max_Heap_Type) return T;
   procedure Pop_Max (heap: in out Max_Heap_Type; maximum : out T);
   procedure Insert (heap : in out Max_Heap_Type; value : in T);
   procedure Compact (heap : in out Max_Heap_Type; max_size : Integer := -1);
   function Debug_Image (heap : in Max_Heap_Type) return String;
   function Verify_Heap (heap : in Max_Heap_Type) return Boolean;

private
   package Underlying_Vector is new Vectors (Natural, T);
   type Max_Heap_Type is new Underlying_Vector.Vector with null record;
end Uniq_Max_Heap;