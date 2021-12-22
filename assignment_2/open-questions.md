# Open questions

## Exercise 4

> The only reason we used left recursion is that Happy is more efficient at parsing left-recursive rules; they result in a constant stack-space parser, whereas right-recursive rules require stack space proportional to the length of the list being parsed.

When using the uu-tc library we didn't have this problem.

## Exercise 10

Yes this does matter. Lets take the following pseudocode as example:

```py
a -> a, nothing, nothing, nothing, nothing.
b -> nothing, nothing, nothing, nothing, b.
```

When we call a the stack will look like this for each step.

```py
# Step 0 -- initial state
[a]
# Step 1 -- called function a once
[a, nothing, nothing, nothing, nothing]
# Step 2 -- called function a again
[a, nothing, nothing, nothing, nothing, nothing, nothing, nothing, nothing]
# Step 3 -- called function a again
[a, nothing, nothing, nothing, nothing, nothing, nothing, nothing, nothing, nothing, nothing, nothing, nothing]
```

When we call b the stack will look like this for each step.

```py
# Step 0 -- initial state
[b]
# Step 1 - called function b once
[nothing, nothing, nothing, nothing, b]
# Step 1a -- executed all nothings
[b]
# Step 1b -- called function b again
[nothing, nothing, nothing, nothing, b]
# Step 2 -- executed all nothings and called function b again
[nothing, nothing, nothing, nothing, b]
# Step 3 -- executed all nothings and called function b again
[nothing, nothing, nothing, nothing, b]
```

Aka function b is more efficient space wise.
