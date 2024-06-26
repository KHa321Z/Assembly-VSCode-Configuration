# Assembly-VSCode-Configuration
A Template created for **VSCode** to Edit, Assemble and Debug **.asm** files using **DOSBox** and **NASM** (**AFD** for Debugging).

### Package Details:
- **.vscode Folder** (keybindings, launch and tasks.json)
- **DOSBox Portable**
- **Tools** (NASM for assembling, AFD for debugging)
- **Code Folder with a Sample Code File** (Prints B on the 3rd Line)

## Usage
- Download the Repo as a Zip file from the **<> Code** Button.
- Extract the Zip file anywhere you want.
- Open the Extracted Folder in VSCode as it is.
- Create your **.asm** files anywhere inside this folder (**Code** folder can be used for this purpose as well).
- After writing your assembly code use the shortcuts to assemble, run or debug your code.

**Caution:** Make sure your filenames for **.asm** files do not exceed 8 characters and also that they are present anywhere inside the extracted folder and not outside else there can be issues while assembling them.

### Shortcuts:
- Assemble Only:      `Ctrl + Shift + B`
- Assemble and Link:  `Ctrl + Alt + A`
- Assemble and Run:   `Ctrl + Alt + X`
- Assemble and Debug: `Ctrl + Alt + Z`


### Note:
This configuration has been designed for students studying **COAL** course at **FAST-NUCES**. For **Notepad++** version you can refer to this link [here](https://github.com/ASD0x41/Assembly-Programming-Package/tree/main). Share this Repo if you find it useful >_<

---

**Disclaimer: The given tools belong to their respective owners. I only added a little configuration to handle code editing in vscode while also using dosbox and nasm to assemble their codes throughout their course studies.**
