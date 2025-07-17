with GNAT.IO;

package body Uniq_Max_Heap is

   procedure Heapify (heap : in out Max_Heap_Type; index : in Natural);

   function Left (index : Natural) return Natural with Inline is
   begin
      return index * 2 + 1;
   end Left;

   function Right (index : Natural) return Natural with Inline is
   begin
      return index * 2 + 2;
   end Right;

   function Parent (index : Natural) return Natural with Inline is
   begin
      return (index - 1) / 2;
   end Parent;

   function Size (heap : Max_Heap_Type) return Natural is
   begin
      return Natural (heap.Length);
   end Size;

   function Max (heap : Max_Heap_Type) return T is
   begin
      return Element (heap, 0);
   end Max;

   procedure Pop_Last (heap : in out Max_Heap_Type; last : out T) with Inline
   is
   begin
      last := heap.Last_Element;
      Delete_Last (heap);
   end Pop_Last;

   procedure Pop_Max (heap : in out Max_Heap_Type; maximum : out T) is
      stored_max : T := Max (heap);
      last       : T;
   begin
      maximum := stored_max;
      if Size(heap) > 1 then
         Pop_Last (heap, last);
         Replace_Element (heap, 0, last);
         Heapify (heap, 0);
         while (Size(heap) > 0 and Max (heap) = stored_max) loop
            Pop_Max (heap, stored_max);
         end loop;
      else
         Pop_Last (heap, last);
      end if;
   end Pop_Max;

   procedure Heapify (heap : in out Max_Heap_Type; index : in Natural) is
      l        : constant Natural := Left (index);
      r        : constant Natural := Right (index);
      s        : constant Natural := Size (heap);
      largest : Natural := index;
   begin
      if l < s then
         if Element (heap, l) > Element (heap, index) then
            largest := l;
         end if;
         if r < s then
            if Element (heap, largest) < Element (heap, r) then
               largest := r;
            end if;
         end if;
      end if;
      if largest /= index then
         declare
            temp : T := Element (heap, largest);
         begin
            Replace_Element (heap, largest, Element (heap, index));
            Replace_Element (heap, index, temp);
            Heapify (heap, largest);
         end;
      end if;
   end Heapify;

   procedure Build_Heap (heap : in out Max_Heap_Type) is
      s : constant Natural := Size (heap);
   begin
      for i in reverse 1 .. s / 2 loop
         Heapify (heap, i);
      end loop;
   end Build_Heap;

   procedure Insert (heap : in out Max_Heap_Type; value : in T) is
      index : Natural := Size (heap);
   begin
      Append (heap, value);
      while index > 0
        and then Element (heap, index) > Element (heap, Parent (index))
      loop
         declare
            temp : T := Element (heap, index);
         begin
            Replace_Element (heap, index, Element (heap, Parent (index)));
            index := Parent (index);
            Replace_Element (heap, index, temp);
         end;
      end loop;
   end Insert;

   function Debug_Image (heap : in Max_Heap_Type) return String is
   begin
      return heap'Image;
   end Debug_Image;

   function Verify_Heap
     (heap : in Max_Heap_Type; index : Natural) return Boolean
   is
      s : constant Natural := Size (heap);
      rb : Boolean := true;
   begin
      if Right (index) < s then
         if Element (heap, index) < Element (heap, Right (index)) then
            GNAT.IO.Put_Line
              ("index, value / Right(index), Right value: "
               & index'Image
               & ", "
               & Element (heap, index)'Image
               & " / "
               & Right (index)'Image
               & ", "
               & Element (heap, Right (Index))'Image);
               return false;
         end if;
         rb := Verify_Heap (heap, Right (index));
      end if;
      if Left (index) < s then
         if Element (heap, index) < Element (heap, Left (index)) then
            GNAT.IO.Put_Line
              ("index, value / Left(index), Left value: "
               & index'Image
               & ", "
               & Element (heap, index)'Image
               & " / "
               & Left (index)'Image
               & ", "
               & Element (heap, Left (Index))'Image);
               return false;
         end if;
         return Verify_Heap (heap, Left (index)) and rb;
      end if;
      return rb;
   end Verify_Heap;

   function Verify_Heap (heap : in Max_Heap_Type) return Boolean is
   begin
      return Verify_Heap (heap, 0);
   end Verify_Heap;

   procedure Compact (heap : in out Max_Heap_Type; max_size : Integer := -1) is
      package Vect_Sort is new Underlying_Vector.Generic_Sorting;
      new_heap : Max_Heap_Type;
      len : constant Natural := Natural (Length (heap));
      curr_value : T;
      new_count : Natural := 1;
   begin
      if len < 3 then
         return;
      end if;
      -- Vect_Sort.Sort always sorts low to high, so we
      -- have to run things in reverse
      Vect_Sort.Sort (Underlying_Vector.Vector(heap));
      curr_value := heap.Last_Element;      
      new_heap.append (curr_value);
      for i in reverse 0 .. len - 1 loop
         if curr_value /= Element(heap, i) then
            curr_value := Element(heap, i);
            new_heap.Append (curr_value);
            new_count := @ + 1;
            if max_size /= -1 and then new_count >= max_size then
               heap := new_heap;
               return;
            end if;
         end if;
      end loop;
      heap := new_heap;
   end Compact;

end Uniq_Max_Heap;
