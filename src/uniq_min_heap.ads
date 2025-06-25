with Ada.Containers.Vectors; use Ada.Containers;

generic
   type T is (<>);
package Uniq_Min_Heap is
pragma Preelaborate (Uniq_Min_Heap);
   type Min_Heap_Type is tagged private;
   function Size (heap: Min_Heap_Type) return Natural;
   function Min (heap: Min_Heap_Type) return T;
   procedure Pop_Min (heap: in out Min_Heap_Type; minimum : out T);
   procedure Insert (heap : in out Min_Heap_Type; value : in T);
   procedure Compact (heap : in out Min_Heap_Type; max_size : Integer := -1);
   function Debug_Image (heap : in Min_Heap_Type) return String;
   function Verify_Heap (heap : in Min_Heap_Type) return Boolean;
private
   package Underlying_Vector is new Vectors (Natural, T);
   type Min_Heap_Type is new Underlying_Vector.Vector with null record;
end Uniq_Min_Heap;