# %%
def average(list):
  return sum(list) / len(list)

# %%
def remove_if(function: str,lst: list) -> list:
  new_lst = []
  for x in lst:
    if not(eval(function)):
      new_lst.append(x)
  return new_lst
# %%
numbers = [10,9,42,12,79,3,15,6,28,37]
even = remove_if('x%2==1',numbers)
odd = remove_if('x%2==0',numbers)
print("Even numbers average:", sum(even)/len(even))
print("Odd numbers average:", sum(odd)/len(odd))
# %%
print("numbers above 23:",remove_if('x<=23',numbers))
# %%
print("numbers between 9 and 55:",remove_if('x<9 or x>55',numbers))
# %%
name = 'Fred'
f"He said is name was {name}."
# %%
decimal.Decimal("12.34567")
# %%
lst = [3,56,3,8,7,3,5,3,95,1,2,5,3,8,3]
# %%
remove_if('x==3',lst)
# %%
strings = ["Paris","Montpellier","Lyon","Nantes","Tokyo"]
remove_if('len(x) == 5',strings)
# %%
remove_if('strings.index(x)%2==1',strings)
# %%
test = [1,0,1,1,0,1,0,1,1,1,1,0,1,0,1,0,0,0,1,0,1,0,0,1,0]
remove_if('test.index(x)%2==1',test)