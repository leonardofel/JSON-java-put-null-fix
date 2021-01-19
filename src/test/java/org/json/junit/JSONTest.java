package org.json.junit;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.json.JSONObject;
import org.junit.Test;

public class JSONTest {
    @Test
    public void testConstructor() {
        final JSONObject expected = new JSONObject("{\"myKey\":10}");

        @SuppressWarnings("rawtypes")
        Map myRawC = Collections.singletonMap("myKey", Integer.valueOf(10));
        JSONObject jaRaw = new JSONObject(myRawC);

        Map<String, Object> myCStrObj = Collections.singletonMap("myKey", (Object) Integer.valueOf(10));
        JSONObject jaStrObj = new JSONObject(myCStrObj);

        Map<String, Integer> myCStrInt = Collections.singletonMap("myKey", Integer.valueOf(10));
        JSONObject jaStrInt = new JSONObject(myCStrInt);

        Map<?, ?> myCObjObj = Collections.singletonMap((Object) "myKey", (Object) Integer.valueOf(10));
        JSONObject jaObjObj = new JSONObject(myCObjObj);

        assertTrue("The RAW Collection should give me the same as the Typed Collection", expected.similar(jaRaw));
        assertTrue("The RAW Collection should give me the same as the Typed Collection", expected.similar(jaStrObj));
        assertTrue("The RAW Collection should give me the same as the Typed Collection", expected.similar(jaStrInt));
        assertTrue("The RAW Collection should give me the same as the Typed Collection", expected.similar(jaObjObj));
    }

    @Test
    public void testPutMap() {
        final JSONObject expected = new JSONObject("{\"myMap\":{\"myKey\":10}}");

        @SuppressWarnings("rawtypes")
        Map myRawC = Collections.singletonMap("myKey", Integer.valueOf(10));
        JSONObject jaRaw = new JSONObject();
        jaRaw.put("myMap", myRawC);

        Map<String, Object> myCStrObj = Collections.singletonMap("myKey", (Object) Integer.valueOf(10));
        JSONObject jaStrObj = new JSONObject();
        jaStrObj.put("myMap", myCStrObj);

        Map<String, Integer> myCStrInt = Collections.singletonMap("myKey", Integer.valueOf(10));
        JSONObject jaStrInt = new JSONObject();
        jaStrInt.put("myMap", myCStrInt);

        Map<?, ?> myCObjObj = Collections.singletonMap((Object) "myKey", (Object) Integer.valueOf(10));
        JSONObject jaObjObj = new JSONObject();
        jaObjObj.put("myMap", myCObjObj);

        assertTrue("The RAW Collection should give me the same as the Typed Collection", expected.similar(jaRaw));
        assertTrue("The RAW Collection should give me the same as the Typed Collection", expected.similar(jaStrObj));
        assertTrue("The RAW Collection should give me the same as the Typed Collection", expected.similar(jaStrInt));
        assertTrue("The RAW Collection should give me the same as the Typed Collection", expected.similar(jaObjObj));
    }

    @Test
    public void testMapWithNullValue() {
        Map<String, Object> map = new HashMap<String, Object>();
        map.put("trueKey", Boolean.valueOf(true));
        map.put("falseKey", Boolean.valueOf(false));
        map.put("stringKey", "hello world!");
        map.put("nullKey", null);
        map.put("escapeStringKey", "h\be\tllo w\u1234orld!");
        map.put("intKey", Long.valueOf(42));
        map.put("doubleKey", Double.valueOf(-23.45e67));
        JSONObject jsonObject = new JSONObject(map);

        // validate JSON
        assertTrue("expected \"trueKey\":true", Boolean.TRUE.equals(jsonObject.query("/trueKey")));
        assertTrue("expected \"falseKey\":false", Boolean.FALSE.equals(jsonObject.query("/falseKey")));
        assertTrue("expected \"stringKey\":\"hello world!\"", "hello world!".equals(jsonObject.query("/stringKey")));
        assertTrue("expected \"escapeStringKey\":\"h\be\tllo w\u1234orld!\"", "h\be\tllo w\u1234orld!".equals(jsonObject.query("/escapeStringKey")));
        assertTrue("expected \"intKey\":42", Long.valueOf("42").equals(jsonObject.query("/intKey")));
        assertTrue("expected \"doubleKey\":-23.45e67", Double.valueOf("-23.45e67").equals(jsonObject.query("/doubleKey")));
    }

    @Test
    public void testPutOnceNull() {
        JSONObject jsonObject = new JSONObject();
        jsonObject.putOnce(null, null);
        assertTrue("jsonObject should be empty", jsonObject.isEmpty());
        jsonObject.putOnce("", null);
        assertTrue("jsonObject should NOT be empty", !jsonObject.isEmpty());
    }

    @Test
    public void testPutNull() {
        // put null should remove the item.
        String str = "{\"myKey\": \"myval\"}";
        JSONObject jsonObjectRemove = new JSONObject(str);
        jsonObjectRemove.remove("myKey");
        assertTrue("jsonObject should be empty", jsonObjectRemove.isEmpty());

        JSONObject jsonObjectPutNull = new JSONObject(str);
        jsonObjectPutNull.put("myKey", null);
        assertTrue("jsonObject should NOT be empty", !jsonObjectPutNull.isEmpty());
    }

    @Test(expected = NullPointerException.class)
    public void testPutNullBoolean() {
        // null put key
        JSONObject jsonObject = new JSONObject("{}");
        jsonObject.put(null, false);
        fail("Expected an exception");
    }

    @Test(expected = NullPointerException.class)
    public void testPutNullCollection() {
        // null put key
        JSONObject jsonObject = new JSONObject("{}");
        jsonObject.put(null, Collections.emptySet());
        fail("Expected an exception");
    }

    @Test(expected = NullPointerException.class)
    public void testPutNullDouble() {
        // null put key
        JSONObject jsonObject = new JSONObject("{}");
        jsonObject.put(null, 0.0d);
        fail("Expected an exception");
    }

    @Test(expected = NullPointerException.class)
    public void testPutNullFloat() {
        // null put key
        JSONObject jsonObject = new JSONObject("{}");
        jsonObject.put(null, 0.0f);
        fail("Expected an exception");
    }

    @Test(expected = NullPointerException.class)
    public void testPutNullInt() {
        // null put key
        JSONObject jsonObject = new JSONObject("{}");
        jsonObject.put(null, 0);
        fail("Expected an exception");
    }

    @Test(expected = NullPointerException.class)
    public void testPutNullLong() {
        // null put key
        JSONObject jsonObject = new JSONObject("{}");
        jsonObject.put(null, 0L);
        fail("Expected an exception");
    }

    @Test(expected = NullPointerException.class)
    public void testPutNullMap() {
        // null put key
        JSONObject jsonObject = new JSONObject("{}");
        jsonObject.put(null, Collections.emptyMap());
        fail("Expected an exception");
    }

    @Test(expected = NullPointerException.class)
    public void testPutNullObject() {
        // null put key
        JSONObject jsonObject = new JSONObject("{}");
        jsonObject.put(null, new Object());
        fail("Expected an exception");
    }
}
