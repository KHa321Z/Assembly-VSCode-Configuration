# Assembly-VSCode-Configuration
A Template created for **VSCode** to Edit, Assemble and Debug **.asm** files using **DOSBox** and **NASM** (**AFD** for Debugging).

### Package Details:
- **.vscode Folder** (keybindings, launch and tasks.json)
- **DOSBox Portable**
- **Tools** (NASM for assembling, AFD for debugging)
- **Code Folder with a Sample Code File** (Prints B on the 3rd Line)

## Usage
1. Download the Repo as a Zip file from the **<> Code** Button.
2. Extract the Zip file anywhere you want.
3. Open the Extracted Folder in VSCode as it is.
4. Create your **.asm** files anywhere inside this folder (**Code** folder can be used for this purpose as well).
5. After writing your assembly code use shortcuts to assemble, run or debug your code.

**Caution:** Make sure your filenames for **.asm** files do not exceed 8 characters and also that they are present anywhere inside the extracted folder and not outside, else there can be issues while assembling them.

### Shortcuts:
- Assemble Only:      `Ctrl + Shift + B`
- Assemble and Run:   `Will only work if you Add a Shortcut for Run Test Task`

For Linking and Debugging you'll have to do a workaround unless you have keybinds set up globally. The procedure to link or debug your code is as follows:
1. Press `Ctrl + Shift + P` to open Command Pallete (A Drop-down Menu along with a Text-Field).
2. Write `Run Tasks` and Press `Enter`.
3. You will see 4 Tasks from which you can choose using `Arrow keys` and Pressing `Enter`.
4. The Selected `Task` will then run.


### Note:
This configuration has been designed for students studying **COAL** course at **FAST-NUCES**. For **Notepad++** version you can refer to this link [here](https://github.com/ASD0x41/Assembly-Programming-Package/tree/main). Share this Repo if you find it useful >_<

---

**Disclaimer: The given tools belong to their respective owners. I only added a little configuration to handle code editing in vscode while also using dosbox and nasm to assemble their codes throughout their course studies.**