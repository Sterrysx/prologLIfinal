# CLP(FD) Quick-Reference Cheat Sheet

_All CLP(FD) exercises follow this 4-step strategy:_

1. **Variables & Domains**  
2. **Constraints**  
3. **Labeling (Search)**  
4. **Show the Results**

---

## 1. Variables & Domains

### 1.1 `length/2`  
**Purpose:**  
Generate or inspect the length of a list—often used to create a list of fresh FD variables.  

**Examples & Explanations:**

1. **Create 3 fresh variables**  
   ```prolog
   ?- length(Vars, 3).
   Vars = [_, _, _].
