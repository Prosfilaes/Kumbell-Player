with GNAT.IO;

package body Uniq_Min_Heap is
   procedure Heapify (heap : in out Min_Heap_Type; index : in Natural);

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

   function Size (heap : Min_Heap_Type) return Natural is
   begin
      return Natural (heap.Length);
   end Size;

   function Min (heap : Min_Heap_Type) return T is
   begin
      return Element (heap, 0);
   end Min;

   procedure Pop_Last (heap : in out Min_Heap_Type; last : out T) with Inline
   is
   begin
      last := heap.Last_Element;
      Delete_Last (heap);
   end Pop_Last;

   procedure Pop_Min (heap : in out Min_Heap_Type; minimum : out T) is
      stored_min : T := Min (heap);
      last       : T;
   begin
      minimum := stored_min;
      if Size(heap) > 1 then
         Pop_Last (heap, last);
         Replace_Element (heap, 0, last);
         Heapify (heap, 0);
         while (Size(heap) > 0 and Min (heap) = stored_min) loop
            Pop_Min (heap, stored_min);
         end loop;
      else
         Pop_Last (heap, last);
      end if;
   end Pop_Min;

   procedure Heapify (heap : in out Min_Heap_Type; index : in Natural) is
      l        : constant Natural := Left (index);
      r        : constant Natural := Right (index);
      s        : constant Natural := Size (heap);
      smallest : Natural := index;
   begin
      if l < s then
         if Element (heap, l) < Element (heap, index) then
            smallest := l;
         end if;
         if r < s then
            if Element (heap, smallest) > Element (heap, r) then
               smallest := r;
            end if;
         end if;
      end if;
      if smallest /= index then
         declare
            temp : T := Element (heap, smallest);
         begin
            Replace_Element (heap, smallest, Element (heap, index));
            Replace_Element (heap, index, temp);
            Heapify (heap, smallest);
         end;
      end if;
   end Heapify;

   procedure Build_Heap (heap : in out Min_Heap_Type) is
      s : constant Natural := Size (heap);
   begin
      for i in reverse 1 .. s / 2 loop
         Heapify (heap, i);
      end loop;
   end Build_Heap;

   procedure Insert (heap : in out Min_Heap_Type; value : in T) is
      index : Natural := Size (heap);
   begin
      Append (heap, value);
      while index > 0
        and then Element (heap, index) < Element (heap, Parent (index))
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

   function Debug_Image (heap : in Min_Heap_Type) return String is
   begin
      return heap'Image;
   end Debug_Image;

   function Verify_Heap
     (heap : in Min_Heap_Type; index : Natural) return Boolean
   is
      s : constant Natural := Size (heap);
      rb : Boolean := true;
   begin
      if Right (index) < s then
         if Element (heap, index) > Element (heap, Right (index)) then
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
         if Element (heap, index) > Element (heap, Left (index)) then
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

   function Verify_Heap (heap : in Min_Heap_Type) return Boolean is
   begin
      return Verify_Heap (heap, 0);
   end Verify_Heap;

   procedure Compact (heap : in out Min_Heap_Type; max_size : Integer := -1) is
      package Vect_Sort is new Underlying_Vector.Generic_Sorting;
      i : Natural := 0;
      new_heap : Min_Heap_Type;
      len : constant Natural := Natural (Length (heap));
      curr_value : T;
      new_count : Natural := 1;
   begin
      if len < 3 then
         return;
      end if;
      Vect_Sort.Sort (Underlying_Vector.Vector(heap));
      curr_value := heap.First_Element;      
      new_heap.append (curr_value);
      while i < len loop
         if curr_value /= Element(heap, i) then
            curr_value := Element(heap, i);
            new_heap.Append (curr_value);
            new_count := @ + 1;
            if max_size /= -1 and then new_count >= max_size then
               heap := new_heap;
               return;
            end if;
         end if;
         i := @ + 1;
      end loop;
      heap := new_heap;
   end Compact;

end Uniq_Min_Heap;
