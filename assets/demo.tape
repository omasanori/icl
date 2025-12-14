# ICL Demo - History and Tab Completion
# Run with: vhs demo.tape

Output assets/demo.gif

Set Shell "bash"
Set FontSize 16
Set Width 800
Set Height 500
Set Theme "Dracula"

Type "icl"
Enter
Sleep 4s

# Type some expressions
Type "(+ 1 2 3)"
Enter
Sleep 1s

Type "(defun square (x) (* x x))"
Enter
Sleep 1s

Type "(square 5)"
Enter
Sleep 1s

Type "(mapcar #'square '(1 2 3 4 5))"
Enter
Sleep 1.5s

Type "(defun foo (x y)"
Enter
Sleep 500ms
Type "(+ x y))"
Enter

Sleep 1s
Type "(foo 6 7)"
Enter

# Demonstrate history with up arrow
Sleep 500ms
Up
Sleep 500ms
Up
Sleep 500ms
Up
Sleep 500ms
Type "d"
Enter
Sleep 2s

# Tab complete after colon
Type "(string-"
Sleep 300ms
Tab
Sleep 1s
Down
Sleep 500ms
Down
Sleep 500ms
Down
Sleep 500ms
Tab
Sleep 500ms
Type ' "A" "B")'
Enter
Sleep 4


