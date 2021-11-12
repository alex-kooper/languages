# Facebook Sample Interview Question "Edit Distance"
#
# Write a function that returns whether two words are exactly "one edit" away
# using the following signature:
#
# bool OneEditApart(string s1, string s2);
#
# An edit is:
#
# Inserting one character anywhere in the word (including at the beginning and end)
# Removing one character
# Replacing one character
#
# Examples:
#
# OneEditApart("cat", "dog") = false
# OneEditApart("cat", "cats") = true
# OneEditApart("cat", "cut") = true
# OneEditApart("cat", "cast") = true
# OneEditApart("cat", "at") = true
# OneEditApart("cat", "act") = false

def one_edit_apart(s1, s2):
    if abs(len(s1) - len(s2)) > 1:
        return False

    if not s1 or not s2:
        return True
    
    if s1[0] == s2[0]:
        return one_edit_apart(s1[1:], s2[1:])

    if len(s1) > len(s2):
        return s1[1:] == s2
    
    if len(s2) > len(s1):
        return s2[1:] == s1
    
    return s1[1:] == s2[1:]
