import java.util.ArrayList;

public class LispLexer{
    public static void main(String[] args) {
        
        String expression = "(blabla x y)";
        
        boolean esCorrecta = verificarParentesis(expression);
        if (esCorrecta) {
            System.out.println("Expresión correcta");
        } else {
            System.out.println("Expresión incorrecta");
        }

        ArrayList<String> tokens = tokenizar(expression);
        System.out.print("Tokens: ");
        for (int i = 0; i < tokens.size(); i++) {
            System.out.print(tokens.get(i));
            if (i < tokens.size() - 1) {
                System.out.print(", ");
            }
        }
        System.out.println();
    }
 
    public static boolean verificarParentesis(String expr) {
        int contador = 0; 
        int i = 0;
        
        
        while (i < expr.length()) {
            char caracter = expr.charAt(i); 
            if (caracter == '(') {
                contador = contador + 1; 
            } else if (caracter == ')') {
                contador = contador - 1; 
                if (contador < 0) { 
                    return false; 
                }
            }
            i = i + 1;
        }
        
        
        if (contador == 0) {
            return true;
        } else {
            return false; 
        }
    }

    
    public static ArrayList<String> tokenizar(String expr) {
        ArrayList<String> listaTokens = new ArrayList<String>(); 
        String tokenActual = "";
        int i = 0;
        
        
        while (i < expr.length()) {
            char caracter = expr.charAt(i);
            
           
            if (caracter == '(' || caracter == ')') {
               
                if (tokenActual.length() > 0) {
                    listaTokens.add(tokenActual);
                    tokenActual = "";
                }
                
                listaTokens.add("" + caracter);
            } else if (caracter == ' ') {
              
                if (tokenActual.length() > 0) {
                    listaTokens.add(tokenActual);
                    tokenActual = "";
                }
               
            } else {
               
                tokenActual = tokenActual + caracter;
            }
            i = i + 1; 
        }
        
        
        if (tokenActual.length() > 0) {
            listaTokens.add(tokenActual);
        }
        
        return listaTokens; 
    }
}

/*
Gork 3.0 [Hola gork, debo hacer un código en Java que hace analisis léxico de un lisp.  El programa debe recibir una expresión LISP, como por ejemplo:(- 9 (* 3 5)) y devolver: 
a) si la expresión es correcta porque tiene la misma cantidad de paréntesis que abre y que cierran, o la expresión es incorrecta.
b) la lista de todos los elementos de la expresion (tokens): (,-, 9, (, *, 3, 5,),)
ahora mismo tengo un código pero tengo dudas de que todas sus lineas sean lo mas óptimo para hacer este lexer. Aquí está el código que tengo ahora mismo, 
me gustaría que lo usaras de base para corregirlo y explicarme la lógica detrás de tus correxiones para que funcione bien manteniendo el lenguaje de java básico
que utilizamos para hacerlo originalmente]
Guatemala, recabado en 28 de febrero de 2025
*/


