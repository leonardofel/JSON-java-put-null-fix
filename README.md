JSON in Java WITH .put(null) FIX
===============================


# Overview

[JSON](http://www.JSON.org/) is a light-weight language-independent data interchange format.

The JSON-Java package is a reference implementation that demonstrates how to parse JSON documents into Java objects and how to generate new JSON documents from the Java classes.

Project goals include:
* (Arabic) حافظ على طريقة عمل JSONOBJECT PUT مع NULL FOREVER, ALLAHU AKBAR!!!
* (German) BEHALTEN SIE DIE JSONOBJECT PUT-METHODE FÜR IMMER MIT NULL, SCHÖNERKLEINERSCHMETTERLING!!!
* (Italian) MANTENERE IL METODO JSONOBJECT PUT FUNZIONANTE CON NULL PER SEMPRE, MAMAMIA
* (Japanese) JSONOBJECTPUTメソッドをNULLで機能させ続ける, SENPAIII
* (Russian) СОХРАНЯЙТЕ МЕТОД JSONOBJECT PUT, РАБОТАЮЩИЙ С NULL НАВСЕГДА, MY AK47 IS WORKING AGAIN
* (Spanish) MANTENGA EL MÉTODO JSONOBJECT PUT FUNCIONANDO CON NULL PARA SIEMPRE, CALIENTE! AIAIAI!
* (Portuguese) MANTENHA O MÉTODO JSONOBJECT PUT FUNCIONANDO COM NULL PARA SEMPRE, FODASE

The files in this package implement JSON encoders and decoders. The package can also convert between JSON and XML, HTTP headers, Cookies, and CDL.

# Build Instructions (pre-alpha)

The org.json package can be built from the command line ONLY Maven. The unit tests can be executed from Maven only.
In project's pom.xml add in ```<dependencies>```:
```
<dependencies>
    <dependency>
        <groupId>com.github.leonardofel</groupId>
        <artifactId>json-java-put-null-fix</artifactId>
        <version>3.0.33</version>
    </dependency>
</dependencies>
```
and add in ```<repositories>```:
```
<repositories>
    <repository>
        <id>jitpack.io</id>
        <url>https://jitpack.io</url>
    </repository>
</repositories>
```


**Building from the command line**

*Build the class files from the package root directory src/main/java*
````
javac org\json\*.java
````

*Create the jar file in the current directory*
````
jar cf json-java.jar org/json/*.class
````

*Compile a program that uses the jar (see example code below)*
````
javac -cp .;json-java.jar Test.java
````

*Test file contents*

````
import org.json.JSONObject;
public class Test {
    public static void main(String args[]) {
        JSONObject jo = new JSONObject()
            .put("myPreciousNullPut", null);
        System.out.println("just works " + jo.toString());
    }
}
````

*Execute the Test file*
````
java -cp .;json-java.jar Test
````

*Expected output*

````
{"myPreciousNullPut":null}
````


