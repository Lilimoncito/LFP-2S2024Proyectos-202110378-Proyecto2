import tkinter as tk
import graphviz
from tkinter import scrolledtext, filedialog, Menu, ttk
import webbrowser
import subprocess

class Analizador_App:

    def __init__(self,root):
        self.root = root
        self.root.title("Analizador_Lexico")

        self.frame_info = tk.Frame(self.root)
        self.frame_info.pack(side=tk.LEFT, padx=10)
        
        #cuadro de texto para pegar el texto
        self.cuadro_texto = scrolledtext.ScrolledText(self.root, wrap = tk.WORD, width = 50, height = 10 )
        self.cuadro_texto.pack(pady=10)
        #boton analizar
        self.boton_analizar = tk.Button(self.root, text = "analizar", command= self.analizar_texto)
        self.boton_analizar.pack(pady = 10)
         # Crear la barra de menú
        self.menu_bar = Menu(self.root, tearoff=0)
        self.root.config(menu=self.menu_bar)

        # Menú "Archivo"
        archivo_menu = Menu(self.menu_bar, tearoff=0)
        self.menu_bar.add_cascade(label="Menu", menu=archivo_menu)
        self.menu_bar.add_command(label = 'Acerca de', command=self.abrir_ventana_texto)
        self.menu_bar.add_command(label = 'Salir', command= self.salir_aplicacion)
        self.menu_bar.add_command(label = 'Tokens', command= self.mostrar_errores_en_pestania)
        archivo_menu.add_command(label="Abrir archivo", command=self.abrir_archivo)
        archivo_menu.add_command(label="Guardar", command=self.guardar_archivo)
        archivo_menu.add_command(label="Guardar como", command=self.guardar_como_archivo)
        archivo_menu.add_command(label="Abrir archivo", command=self.abrir_archivo)

        # Frame para la tabla
        self.frame_tabla = tk.Frame(self.root)
        self.frame_tabla.pack(pady=10)

        # Configurar Treeview
        self.tree = ttk.Treeview(self.frame_tabla, columns=("Tipo", "Valor"), show="headings")
        self.tree.heading("Tipo", text="Tipo")
        self.tree.heading("Valor", text="Valor")
        self.tree.pack()

        self.frame_tabla_errores = tk.Frame(self.root)
        
        # Configurar Treeview para mostrar errores
        self.tree_errores = ttk.Treeview(self.frame_tabla_errores, columns=("Descripcion", "linea"), show="headings")
        self.tree_errores.heading("Descripcion", text="Descripcion")
        self.tree_errores.heading("linea", text="linea")
        
        
        # Inicialmente ocultamos el frame de la tabla de errores
        self.frame_tabla_errores.pack_forget()

        self.filename = None
        self.label_imagen_sat = None
        self.label_info_sat = None
        self.label_grafica = None
    def salir_aplicacion(self):
        self.root.quit()
    def abrir_ventana_texto(self):
        ventana_texto = tk.Toplevel(self.root)
        ventana_texto.title("Datos del estudiante")

        texto = "Datos del estudiante: \n Nombre:Cesar Armando Garcia Franco \n Carnet:202110378 \n Curso: Lenguajes Formales de Programacion Seccion B+"

        # Agregar un Label con el texto
        label_texto = tk.Label(ventana_texto, text=texto, padx=10, pady=10)
        label_texto.pack()

        # Agregar un botón para cerrar la ventana
        boton_cerrar = tk.Button(ventana_texto, text="Cerrar", command=ventana_texto.destroy)
        boton_cerrar.pack(pady=10)
    def analizar_texto(self):
        texto = self.cuadro_texto.get("1.0", tk.END).strip()
        print (f"texto a analizar:" , texto)

        with open("entrada.txt", "w") as f:
            f.write(texto)

        try:
            result = subprocess.run([r"C:\Users\cesar\OneDrive\Documentos\Fortran\Proyecto 2\Fortran\Main.exe", "entrada.txt"], capture_output=True, text = True)
            print (f"salida del analizador: {result.stdout}")
            print (f"errores: {result.stderr}")

            self.mostrar_tokens('salidaTokens.txt')
        except FileNotFoundError:
            print("no se encontro el archivo ejecutable")

    def mostrar_tokens(self, archivo):
        # Limpiar la tabla antes de agregar nuevos tokens
        for item in self.tree.get_children():
            self.tree.delete(item)

        # Procesar los tokens
        with open(archivo, "r") as f:
            lines = f.readlines()  # Leer todas las líneas del archivo
            tipo = None
            for line in lines:
                if line.startswith("Tipo:"):
                    tipo = line.split(":")[1].strip()
                elif line.startswith("valor:"):
                    valor = line.split(":")[1].strip()
                    self.tree.insert("", "end", values=(tipo, valor))
    def mostrar_errores(self, archivo):
        # Limpiar la tabla de errores antes de agregar nuevos
         # Limpiar la tabla antes de agregar nuevos tokens
        for item in self.tree_errores.get_children():
            self.tree_errores.delete(item)


        # Procesar los tokens
        with open(archivo, "r") as f:
            lines = f.readlines()  # Leer todas las líneas del archivo
            tipo = None
            for line in lines:
                if line.startswith("Tipo:"):
                    tipo = line.split(":")[1].strip()
                elif line.startswith("linea:"):
                    valor = line.split(":")[1].strip()
                    self.tree_errores.insert("", "end", values=(tipo, valor))

    def mostrar_errores_en_pestania(self):
        # Mostrar solo la tabla de errores al seleccionar "Errores" en el menú
        self.frame_tabla_errores.pack(pady=10)  # Asegura que el frame sea visible
        self.tree_errores.pack(side=tk.LEFT, padx=10)

        # Llamar a la función para mostrar errores
        self.mostrar_errores("listaErrores.txt")

    def abrir_archivo(self):
        # Abrir un cuadro de diálogo para seleccionar un archivo .org
        archivo = filedialog.askopenfilename(title="Abrir archivo", filetypes=[("Archivos .org", "*.org")])
        if archivo:
            with open(archivo, "r") as f:
                contenido = f.read()
                self.cuadro_texto.delete("1.0", tk.END)  # Limpiar el cuadro de texto
                self.cuadro_texto.insert(tk.END, contenido)  # Insertar el contenido del archivo

    def guardar_archivo(self):
        if self.filename:  # Si ya hay un archivo abierto
            with open(self.filename, 'w') as f:
                f.write(self.cuadro_texto.get("1.0", tk.END))
        else:
            self.guardar_como_archivo()  # Llama a guardar como si no hay archivo

    def guardar_como_archivo(self):
        archivo = filedialog.asksaveasfilename(defaultextension=".org", filetypes=[("Archivos .org", "*.org"), ("Todos los archivos", "*.*")])
        if archivo:
            with open(archivo, 'w') as f:
                f.write(self.cuadro_texto.get("1.0", tk.END))
            self.filename = archivo  # Actualizar el nombre del archivo actual
    
if __name__ == "__main__":
    root = tk.Tk()
    app = Analizador_App(root)
    root.mainloop()


