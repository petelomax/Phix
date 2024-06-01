--
-- builtins\repeat.e
-- =================
--
--  Implements repeat() and repeatch(). [Migrated from builtins\VM\pRepeatN.e for version 1.0.0]
--
--  The repeatch() function returns a string and helps with type propagation.
--  The compiler automatically maps repeat() to repeatch() whenever it can 
--   (and that should be a completely transparent operation).
--
--  Also implements deep_copy(), for the benefit of pwa/p2js.
--
without debug
--with debug
--
--  Since JavaScript has pass-by-sharing semantics, whereas Phix has pass-by-reference-with-copy-on-write semantics,
--  and implementing either in the other is sheer madness, this enables software depending on neither to be written.
--
--  A "with js" aka "with javascript_semantics" directive causes the desktop interpreter (and probably compiler, but 
--  who really cares) to trigger a fatal error whenever any copy-on-write event (aka internal clone) triggers. 
--
--  Obviously that enables software to be written that does not depend on copy-on-write, and in turn to not generate 
--  any pass-by-sharing-side-effect-laden JavaScript. Note this particular source is not affected in any way by with
--  or without js, however repeat() is probably ultimately responsible for over 50% of all copy-on-write events, and
--  since that needs to use deep_copy(), it makes sense to bundle all these things together in one place here.
--
--  Technical details
--  =================
--
--      The desktop/Phix compiler defaults to "without js" and ignores that directive, whereas a "with js" directive
--      invokes %pWithJS (eax:=2 aka 0b10) to set an internal flag in pHeap.e, for use by %pAllocClone. It will also
--      terminate in error on encountering a mixture of with and without js directives (in different include files).
--      Neither "with js" nor "without js" prohibits running on the desktop, but the former applies runtime checks.
--      Also note that "with js" does not in any way prohit the use of say dir(), rather it assumes that any such 
--      web-browser-unfriendly calls are only made from within appropriate platform() checks.
--
--      Conversely pwa/p2js defaults to "with js" and ignores that directive, whereas a "without js" directive causes
--      a transpile-time error and that way prevents any (potentially misleading) output at all from being generated.
--
--      About a dozen selected call :%pAllocSeq now first invoke call :%pAlloClone which checks whether with js is
--      in force, and raises a runtime error ("p2js violation: relies on copy on write semantics") as needed.
--      Note that error (e56rocow) is deemed non-catchable, in a similar fashion to e12pa (! keyed/program aborted).
--      (Should that interfere in any way with some critical work in progress, just remove all the "with js")
--      Apart from the changes to repeat, which should be neglible and may even represent a saving, that is from
--      fewer potential cache misses during some delayed internal clones, there should not be any significant 
--      performance impact of any of this on non-with-js code (he says hopefully).
--
--  Usage
--  =====
--          1: Add "with js" or "with javascript_semantics" to the top of the main file.
--          2: Write code normally, but expect the odd new "cow semantics" fatal error.
--          3: Rewrite and/or wrap any offenders detected in a deep_copy() as needed.
--
--      Refer to builtins\psqop.e for some examples of code rewritten to be compliant (w/o using deep_copy).
--      Obviously, examples of code that now uses deep_copy() can be easily found using a standard search.
--
--      Sometimes, you may want to avoid modifying parameters, eg if the main loop below was setting o[i] instead 
--      of res[i], it would probably violate its own standards (but other very similar code exists). 
--      However when (always) safely used in an o = deep_copy(o) fashion, such that automatic pbr kicks in and 
--      the refcount of o is 1 on entry, precisely that c/would deliver the best performance. 
--      (Obviously we cannot possibly make any such assumption about this specific routine itself.)
--      In other words, code that looks like it relies on cow semantics, but actually does not due to the way in
--      which it is called, may still be the best and most efficient way to write it, sometimes.
--
--  With a bucket of luck and a following wind, there might not be anything much of a
--  noticeable performance hit, at other times it could be tragic and merit a rethink.
--
--global function deep_copy(object o, integer depth=-1, bool bIfNeeded=false)
global function deep_copy(object o)
    --
    -- Clone o, by default making a perfectly independant copy.
    --
    -- Not auto-transpiled, there is an equivalent hand-crafted one in p2js.js.
    --
    -- If depth is -1, it will just decrement ad infinitum, cloning everything.
    -- A positive depth limits cloning, with 1 being top-level only. 
    -- In many cases you don't actually need to clone any deeper than that, or 
    -- you can "clone a path" to the thing you want to change, for example to 
    -- modify a[1][2][3], clone toplevel a, then a[1], then a[1][2] and lastly
    -- set [3] on that, which is the same "magic" that has happened behind the
    -- scenes since day 1, to originally implement copy-on-write semantics.
    -- In other words use deep_copy to explicitly perform the same operations
    -- that were previously implicitly performed under cow-semantics, as close
    -- to that as you can reasonably manage, that is.
    --
    -- bIfNeeded may or may not be helpful. A refcount of 1 can only occur when
    -- called as o = deep_copy(o) and pbr applies, or o = deep_copy(expr) where
    -- expr has been freshly evaluated into an unnamed temp. Note that it is not 
    -- reapplied recursively: if you needed to clone that would have bumped up
    -- the refcount, or will rsn, making nested checks pointless or just wrong.
    -- Obviously there will be cases where cloning is never needed, so you did
    -- not actually need to call this routine at all, and other cases where it
    -- is always needed, which would make any check for it quite unnecessary.
    -- Also note that JavaScript does not have a reference count, so obviously
    -- bIfNeeded cannot be implemented there, and will just be quietly ignored.
    --
    assert(not still_has_delete_routine(o)) -- 13/4/24 (!!!)
    if sequence(o) and not string(o) then
--      assert(not still_has_delete_routine(o)) -- 13/4/24 (!!!)
--/*
        -- see also below, could be improved on by checking the
        --  "with js" flag directly (when bIfNeeded set or not).
        if bIfNeeded then
            integer refcount
            #ilASM{
                [32]
                    mov eax,[o]
                    mov eax,[ebx+eax*4-8]
                    mov [refcount],eax
                [64]
                    mov rax,[o]
                    mov rax,[rbx+rax*4-16]
                    mov [refcount],rax
                []
                  }
            if refcount=1 then
                depth = 0
            end if
        end if
        if depth then
            depth -= 1
--*/
            integer l = length(o)
            sequence res = repeat(0,l)
            for i=1 to l do
--              -- aside: don't pass bIfNeeded here, since it cannot possibly
--              --        be useful, or perhaps I should say ever be correct.
--              res[i] = deep_copy(o[i],depth)
                res[i] = deep_copy(o[i])
            end for
            return res
--      end if
    end if
    return o
end function

global function repeatch(integer ch, n, nFrames=2)
    --
    -- returns a string. Note the compiler automatically maps eg repeat('=',80) to
    --  repeatch('=',80), and consequently improves type inference. It will also
    --  map repeat('\0',80) to repeatch (ie using toktype=SQUOTE). Obviously
    --  without appropriate clues, the dseq/str decision is made at run-time.
    --
    -- nFrames is for the benefit of repeat(), to point crash msg at user code.
    --
    if n<0 or ch!=and_bits(ch,#FF) then
        crash("repeat count must not be negative",{},nFrames)
    end if
    string res
    -- create a string (one byte per character/raw binary)
    #ilASM {
        [32]
            mov ecx,[n]
            call :%pAllocStr
            mov [res],eax
            lea edi,[ebx+eax*4]
            mov ecx,[n]
            mov eax,[ch]
            rep stosb
            mov [edi],bl
        [64]
            mov rcx,[n]
            call :%pAllocStr
            mov [res],rax
            lea rdi,[rbx+rax*4]
            mov rcx,[n]
            mov rax,[ch]
            rep stosb
            mov [rdi],bl
        []
          }
    return res
end function

global function repeat(object x, integer n, bool allow_strings = true)
    --
    -- Note: Prior versions of say repeat(repeat(100),100) would use just 800ish bytes and
    --       an inner refcount of 100, vs 40,000 bytes fully expanded. However apart maybe
    --       from a faster startup, that was always a "fake saving", that any real program 
    --       would soon undo, with the latter probably at a slight overall additional cost.
    --       So now (for p2js) we just grab the full 40K straightaway, and be done with it.
    --       Some "sparse array" programs may no longer work or run out of memory, by they
    --       were probably better off using a dictionary or similar instead anyway.
    --
    sequence res
    if allow_strings and integer(x) and x>=7 and x<=255 then
        return repeatch(x,n,3)
    elsif n<0 then
        return repeatch('?',n,3) -- (crash)
--  elsif n=0 then
--      res = {}
--  else
    end if
    bool bDeep = (sequence(x) and not string(x))
    object x0 = iff(bDeep?0:x)
    -- create a d/qword-sequence
    #ilASM {
        [32]
            mov ecx,[n]
            call :%pAllocSeq
            mov [res],eax
            lea edi,[ebx+eax*4]
            mov eax,[x0]
            mov ecx,[n]
            cmp eax,h4
            jl @f
                add dword[ebx+eax*4-8],ecx
          @@:
            rep stosd
        [64]
            mov rcx,[n]
            call :%pAllocSeq
            mov [res],rax
            lea rdi,[rbx+rax*4]
            mov rax,[x0]
            mov rcx,[n]
            mov r15,h4
            cmp rax,r15
            jl @f
                add qword[rbx+rax*4-16],rcx
          @@:
            rep stosq
        []
          }
    if bDeep then
        for i=1 to n do
            res[i] = deep_copy(x)
        end for
    end if
    return res
end function
