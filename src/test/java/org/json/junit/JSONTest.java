package org.json.junit;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import org.json.JSONException;
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

    @Test
    public void testNullBoolean() {
        JSONObject jsonObject = new JSONObject("{}");

        final Boolean put_value_1 = false;
        final Boolean put_value_2 = null;

        jsonObject.put("value_1", put_value_1);
        jsonObject.put("value_2", put_value_2);

        final Boolean get_value_1 = jsonObject.getBoolean("value_1");
        final Boolean get_value_2 = jsonObject.optBoolean("value_2");

        assertEquals(put_value_1, get_value_1);
        //assertEquals(put_value_2, get_value_2);

        jsonObject.put("value_1", get_value_1);
        jsonObject.put("value_2", get_value_2);

        final Boolean get_get_value_1 = jsonObject.getBoolean("value_1");
        final Boolean get_get_value_2 = jsonObject.optBoolean("value_2");

        assertEquals(get_get_value_1, get_value_1);
        assertEquals(get_get_value_2, get_value_2);
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

    @Test
    public void testNullDouble() {
        JSONObject jsonObject = new JSONObject("{}");

        final Double put_value_1 = 0.0d;
        final Double put_value_2 = null;

        jsonObject.put("value_1", put_value_1);
        jsonObject.put("value_2", put_value_2);

        final Double get_value_1 = jsonObject.getDouble("value_1");
        final Double get_value_2 = jsonObject.optDouble("value_2");

        assertEquals(put_value_1, get_value_1);
        assertEquals(put_value_2, get_value_2);

        jsonObject.put("value_1", get_value_1);
        jsonObject.put("value_2", get_value_2);

        final Double get_get_value_1 = jsonObject.getDouble("value_1");
        final Double get_get_value_2 = jsonObject.optDouble("value_2");

        assertEquals(get_get_value_1, get_value_1);
        assertEquals(get_get_value_2, get_value_2);
    }

    @Test(expected = NullPointerException.class)
    public void testPutNullDouble() {
        // null put key
        JSONObject jsonObject = new JSONObject("{}");
        jsonObject.put(null, 0.0d);
        fail("Expected an exception");
    }

    @Test
    public void testNullFloat() {
        JSONObject jsonObject = new JSONObject("{}");

        final Float put_value_1 = 0.0f;
        final Float put_value_2 = null;

        jsonObject.put("value_1", put_value_1);
        jsonObject.put("value_2", put_value_2);

        final Float get_value_1 = jsonObject.getFloat("value_1");
        final Float get_value_2 = jsonObject.optFloat("value_2");

        assertEquals(put_value_1, get_value_1);
        assertEquals(put_value_2, get_value_2);

        jsonObject.put("value_1", get_value_1);
        jsonObject.put("value_2", get_value_2);

        final Float get_get_value_1 = jsonObject.getFloat("value_1");
        final Float get_get_value_2 = jsonObject.optFloat("value_2");

        assertEquals(get_get_value_1, get_value_1);
        assertEquals(get_get_value_2, get_value_2);
    }

    @Test(expected = NullPointerException.class)
    public void testPutNullFloat() {
        // null put key
        JSONObject jsonObject = new JSONObject("{}");
        jsonObject.put(null, 0.0f);
        fail("Expected an exception");
    }

    @Test
    public void testNullInteger() {
        JSONObject jsonObject = new JSONObject("{}");

        final Integer put_value_1 = 0;
        final Integer put_value_2 = null;

        jsonObject.put("value_1", put_value_1);
        jsonObject.put("value_2", put_value_2);

        final Integer get_value_1 = jsonObject.getInteger("value_1");
        final Integer get_value_2 = jsonObject.optInteger("value_2");

        assertEquals(put_value_1, get_value_1);
        assertEquals(put_value_2, get_value_2);

        jsonObject.put("value_1", get_value_1);
        jsonObject.put("value_2", get_value_2);

        final Integer get_get_value_1 = jsonObject.getInteger("value_1");
        final Integer get_get_value_2 = jsonObject.optInteger("value_2");

        assertEquals(get_get_value_1, get_value_1);
        assertEquals(get_get_value_2, get_value_2);
    }

    @Test(expected = NullPointerException.class)
    public void testPutNullInt() {
        // null put key
        JSONObject jsonObject = new JSONObject("{}");
        jsonObject.put(null, 0);
        fail("Expected an exception");
    }

    @Test
    public void testNullLong() {
        JSONObject jsonObject = new JSONObject("{}");

        final Long put_value_1 = 0L;
        final Long put_value_2 = null;

        jsonObject.put("value_1", put_value_1);
        jsonObject.put("value_2", put_value_2);

        final Long get_value_1 = jsonObject.getLong("value_1");
        final Long get_value_2 = jsonObject.optLong("value_2");

        assertEquals(put_value_1, get_value_1);
        assertEquals(put_value_2, get_value_2);

        jsonObject.put("value_1", get_value_1);
        jsonObject.put("value_2", get_value_2);

        final Long get_get_value_1 = jsonObject.getLong("value_1");
        final Long get_get_value_2 = jsonObject.optLong("value_2");

        assertEquals(get_get_value_1, get_value_1);
        assertEquals(get_get_value_2, get_value_2);
    }

    @Test(expected = NullPointerException.class)
    public void testPutNullLong() {
        // null put key
        JSONObject jsonObject = new JSONObject("{}");
        jsonObject.put(null, 0L);
        fail("Expected an exception");
    }

    @Test
    public void testNullMap() {
        JSONObject jsonObject = new JSONObject("{}");

        final Map<String, Object> put_value_1 = Collections.emptyMap();
        final Map<String, Object> put_value_2 = null;

        jsonObject.put("value_1", put_value_1);
        jsonObject.put("value_2", put_value_2);

        final Map<String, Object> get_value_1 = jsonObject.getJSONObject("value_1").toMap();
        //final Map<String, Object> get_value_2 = jsonObject.getJSONObject("value_2");

        assertEquals(put_value_1, get_value_1);
        //assertEquals(put_value_2, jsonObject.getJSONObject("value_2").toMap());

        jsonObject.put("value_1", get_value_1);
        //jsonObject.put("value_2", get_value_2);

        final Map<String, Object> get_get_value_1 = jsonObject.getJSONObject("value_1").toMap();
        //final Map<String, Object> get_get_value_2 = jsonObject.toMap();

        assertEquals(get_get_value_1, get_value_1);
        //assertEquals(get_get_value_2, get_value_2);
    }

    @Test(expected = NullPointerException.class)
    public void testPutNullMap() {
        // null put key
        JSONObject jsonObject = new JSONObject("{}");
        jsonObject.put(null, Collections.emptyMap());
        fail("Expected an exception");
    }

    @Test
    public void testNullObject() {
        JSONObject jsonObject = new JSONObject("{}");

        final Object put_value_1 = new Object();
        final Object put_value_2 = null;

        jsonObject.put("value_1", put_value_1);
        jsonObject.put("value_2", put_value_2);

        final Object get_value_1 = jsonObject.get("value_1");
        final Object get_value_2 = jsonObject.opt("value_2");

        assertEquals(put_value_1, get_value_1);
        assertEquals(put_value_2, get_value_2);

        jsonObject.put("value_1", get_value_1);
        jsonObject.put("value_2", get_value_2);

        final Object get_get_value_1 = jsonObject.get("value_1");
        final Object get_get_value_2 = jsonObject.opt("value_2");

        assertEquals(get_get_value_1, get_value_1);
        assertEquals(get_get_value_2, get_value_2);
    }

    @Test(expected = NullPointerException.class)
    public void testPutNullObject() {
        // null put key
        JSONObject jsonObject = new JSONObject("{}");
        jsonObject.put(null, new Object());
        fail("Expected an exception");
    }

    @Test
    public void testPutFloat() {
        // put null should remove the item.
        JSONObject json_0 = new JSONObject()
            .put("myKey_0", "0")
            .put("myKey_1", "1")
            .put("myKey_2", "-1");
        assertTrue("JSONObject getFloat differs from optFloat", json_0.optFloat("myKey_0") == json_0.getFloat("myKey_0"));
        assertTrue("JSONObject getFloat differs from optFloat", json_0.optFloat("myKey_1") == json_0.getFloat("myKey_1"));
        assertTrue("JSONObject getFloat differs from optFloat", json_0.optFloat("myKey_2") == json_0.getFloat("myKey_2"));
        assertTrue("JSONObject getFloat differs from optFloat", json_0.optFloat("myKey_3", -123) == -123.00F);

        JSONObject json_1 = new JSONObject()
            .put("myKey_0", "0.00")
            .put("myKey_1", "1.00")
            .put("myKey_2", "-1.00");
        assertTrue("JSONObject getFloat differs from optFloat", json_1.optFloat("myKey_0") == json_1.getFloat("myKey_0"));
        assertTrue("JSONObject getFloat differs from optFloat", json_1.optFloat("myKey_1") == json_1.getFloat("myKey_1"));
        assertTrue("JSONObject getFloat differs from optFloat", json_1.optFloat("myKey_2") == json_1.getFloat("myKey_2"));
        assertTrue("JSONObject getFloat differs from optFloat", json_1.optFloat("myKey_3", -123) == -123.00F);

        JSONObject json_2 = new JSONObject()
            .put("myKey_0", "00")
            .put("myKey_1", "01")
            .put("myKey_2", "-01");
        assertTrue("JSONObject getFloat differs from optFloat", json_2.optFloat("myKey_0") == json_2.getFloat("myKey_0"));
        assertTrue("JSONObject getFloat differs from optFloat", json_2.optFloat("myKey_1") == json_2.getFloat("myKey_1"));
        assertTrue("JSONObject getFloat differs from optFloat", json_2.optFloat("myKey_2") == json_2.getFloat("myKey_2"));
        assertTrue("JSONObject getFloat differs from optFloat", json_2.optFloat("myKey_3", -123) == -123.00F);

        JSONObject json_3 = new JSONObject()
            .put("myKey_0", "00.00")
            .put("myKey_1", "01.00")
            .put("myKey_2", "-01.00");
        assertTrue("JSONObject getFloat differs from optFloat", json_3.optFloat("myKey_0") == json_3.getFloat("myKey_0"));
        assertTrue("JSONObject getFloat differs from optFloat", json_3.optFloat("myKey_1") == json_3.getFloat("myKey_1"));
        assertTrue("JSONObject getFloat differs from optFloat", json_3.optFloat("myKey_2") == json_3.getFloat("myKey_2"));
        assertTrue("JSONObject getFloat differs from optFloat", json_3.optFloat("myKey_3", -123) == -123.00F);
    }

    @Test
    public void lastTest() {
        //final Byte a_Byte /* = new JSONObject().optByte("a") */;
        //final Double a_Double = new JSONObject().optDouble("a"); //testNullDouble
        //final Float a_Float = new JSONObject().optFloat("a"); //testNullFloat
        //final Integer a_Integer = new JSONObject().optInteger("a"); //testNullInteger
        //final Long a_Long = new JSONObject().optLong("a"); //testNullLong
        //final Short a_Short /* = new JSONObject().getShort("a") */;
        //final BigDecimal a_BigDecimal /* = new JSONObject().getBigDecimal("a") */;
        //final BigInteger a_BigInteger /* = new JSONObject().getBigInteger("a") */;
    }

    @Test
    public void computeTest() {
        JSONObject j = new JSONObject();

        var r1 = j.opt("myKey");
        assertEquals(r1, null);

        j.compute("myKey", (k, v) -> v == null ? "myNull" : "unexpected");

        var r2 = j.opt("myKey");
        assertEquals(r2, "myNull");
    }

    @Test
    public void updateNotEqualsTest() {
        final JSONObject j = new JSONObject();

        assertThrows(JSONException.class, () -> j.update("myMapListener", "propertyChange"));

        j.addUpdateListenerGlobal(evt -> {
            final Object oldValue = evt.getOldValue();
            assertEquals("{}", oldValue.toString());
        });

        j.addUpdateListenerGlobal(evt -> {
            final Object newValue = evt.getNewValue();
            assertEquals("{\"myMapListener\":\"propertyChange\"}", newValue.toString());
        });

        j.addUpdateListener("myMapListener", evt -> {
            final Object oldValue = evt.getOldValue();
            final Object newValue = evt.getNewValue();

            assertNotEquals(oldValue, newValue);
        });

        j.update("myMapListener", "propertyChange");
    }

    @Test
    public void updateListenerGlobalTest() {
        final JSONObject j = new JSONObject();

        final AtomicInteger counter = new AtomicInteger();
        final AtomicInteger globalExecutions = new AtomicInteger();
        assertEquals(counter.get(), globalExecutions.get());

        j.addUpdateListenerGlobal(evt -> {
            globalExecutions.incrementAndGet();
        });

        j.addUpdateListenerGlobal(evt -> {
            assertEquals(counter.incrementAndGet(), globalExecutions.get() * 3 - 2);
        });

        j.addUpdateListenerGlobal(evt -> {
            assertEquals(counter.incrementAndGet(), globalExecutions.get() * 3 - 1);
        });

        j.addUpdateListener("myMapListener", evt -> {
            assertEquals(counter.incrementAndGet(), globalExecutions.get() * 3 - 0);
        });

        j.update("myMapListener", "propertyChange123");
        j.update("myMapListener", "propertyChange456");
        j.update("myMapListener", "propertyChange789");

        assertEquals(counter.get(), globalExecutions.get() * 3);
    }

    @Test
    public void updateListenerTest() {
        final JSONObject j = new JSONObject();

        j.put("myMapListener", "unchangedProperty");

        j.addUpdateListener("myMapListener", evt -> {
            fail("They are the same");
        });

        j.update("myMapListener", "unchangedProperty");

        j.addUpdateListener("otherMapListener", evt -> {
            final Object oldValue = evt.getOldValue();
            assertEquals(oldValue, null);

            final Object newValue = evt.getNewValue();
            assertEquals(newValue, "otherOtherPropertyChange");
        });

        j.update("otherMapListener", "otherOtherPropertyChange");
    }

    @Test
    public void updateListener2Test() {
        final JSONObject jsonObject1 = new JSONObject();
        final JSONObject jsonObject2 = new JSONObject();

        jsonObject1
            .put("trueKey", Boolean.valueOf(true))
            .put("falseKey", Boolean.valueOf(false))
            .put("stringKey", "CHANGE ME!!!")
            .put("nullKey", null)
            .put("nullBefore", null)
            .put("nullAfter", "null");

        final JSONObject oldJsonObject1 = new JSONObject(jsonObject1.toString());

        jsonObject2
            .put("nullKey", null)
            .put("nullBefore", "null")
            .put("nullAfter", null)
            .put("stringKey", "hello world!")
            .put("escapeStringKey", "h\be\tllo w\u1234orld!")
            .put("intKey", 42);
            //.put("doubleKey", Double.valueOf(-23.45e67)); PROBLEM WITH DOUBLE CONVERTING TO BIGDECIMAL AFTER JSONOBJECT.TOSTRING

        final JSONObject oldJsonObject2 = new JSONObject(jsonObject2.toString());

        jsonObject1.addUpdateListenerGlobal(evt -> {
            final Object oldValue = evt.getOldValue();
            final Object newValue = evt.getNewValue();

            assertNotEquals(oldValue.toString(), newValue.toString());
        });

        jsonObject1.addUpdateListener("trueKey", evt -> {
            assertEquals(Boolean.valueOf(true), evt.getOldValue());
            assertNull(evt.getNewValue());
        });
        jsonObject1.addUpdateListener("falseKey", evt -> {
            assertEquals(Boolean.valueOf(false), evt.getOldValue());
            assertNull(evt.getNewValue());
        });
        jsonObject1.addUpdateListener("stringKey", evt -> {
            assertEquals("CHANGE ME!!!", evt.getOldValue());
            assertEquals("hello world!", evt.getNewValue());
        });
        jsonObject1.addUpdateListener("nullKey", evt -> {
            assertNull(evt.getOldValue());
            assertNull(null, evt.getNewValue());
        });
        jsonObject1.addUpdateListener("nullBefore", evt -> {
            assertNull(evt.getOldValue());
            assertEquals("null", evt.getNewValue());
        });
        jsonObject1.addUpdateListener("nullAfter", evt -> {
            assertEquals("null", evt.getOldValue());
            assertEquals(null, evt.getNewValue());
        });
        jsonObject1.addUpdateListener("escapeStringKey", evt -> {
            assertNull(evt.getOldValue());
            assertEquals("h\be\tllo w\u1234orld!", evt.getNewValue());
        });
        jsonObject1.addUpdateListener("intKey", evt -> {
            assertNull(evt.getOldValue());
            assertEquals(42, evt.getNewValue());
        });

        assertEquals(jsonObject1.toString(), oldJsonObject1.toString());

        jsonObject1.update(jsonObject2);

        assertNotEquals(jsonObject1.toString(), oldJsonObject1.toString());
        assertEquals(jsonObject2.toString(), oldJsonObject2.toString());

        oldJsonObject1.addUpdateListener("trueKey", evt -> {
            assertEquals(Boolean.valueOf(true), evt.getOldValue());
            assertNull(evt.getNewValue());
        });
        oldJsonObject1.addUpdateListener("falseKey", evt -> {
            assertEquals(Boolean.valueOf(false), evt.getOldValue());
            assertNull(evt.getNewValue());
        });
        oldJsonObject1.addUpdateListener("stringKey", evt -> {
            assertEquals("CHANGE ME!!!", evt.getOldValue());
            assertEquals("hello world!", evt.getNewValue());
        });
        oldJsonObject1.addUpdateListener("nullKey", evt -> {
            assertNull(evt.getOldValue());
            assertNull(null, evt.getNewValue());
        });
        oldJsonObject1.addUpdateListener("nullBefore", evt -> {
            assertNull(evt.getOldValue());
            assertEquals("null", evt.getNewValue());
        });
        oldJsonObject1.addUpdateListener("nullAfter", evt -> {
            assertEquals("null", evt.getOldValue());
            assertEquals(null, evt.getNewValue());
        });
        oldJsonObject1.addUpdateListener("escapeStringKey", evt -> {
            assertNull(evt.getOldValue());
            assertEquals("h\be\tllo w\u1234orld!", evt.getNewValue());
        });
        oldJsonObject1.addUpdateListener("intKey", evt -> {
            assertNull(evt.getOldValue());
            assertEquals(42, evt.getNewValue());
        });

        assertEquals(jsonObject2.toString(), oldJsonObject2.toString());

        oldJsonObject1.updateOrRemove(oldJsonObject2);

        assertNotEquals(jsonObject1.toString(), oldJsonObject1.toString());
        assertEquals(jsonObject2.toString(), oldJsonObject2.toString());
    }

    @Test
    public void updateOrRemoveSrcEmptyTest() {
        try {
            final JSONObject jsonObject1 = new JSONObject();
            final JSONObject jsonObject2 = new JSONObject()
                .put("trueKey", Boolean.valueOf(true))
                .put("falseKey", Boolean.valueOf(false))
                .put("stringKey", "hello world!")
                .put("nullKey", null);

            jsonObject1.addUpdateListenerGlobal(evt -> {
                final Object oldValue = evt.getOldValue();
                final Object newValue = evt.getNewValue();

                assertNotEquals(oldValue.toString(), newValue.toString());
            });

            jsonObject1.updateOrRemove(jsonObject2);
        } catch (Exception ex) {
            ex.printStackTrace();
            fail(ex.getMessage());
        }
    }

    @Test
    public void updateOrRemoveDstEmptyTest() {
        try {
            final JSONObject jsonObject1 = new JSONObject()
                .put("trueKey", Boolean.valueOf(true))
                .put("falseKey", Boolean.valueOf(false))
                .put("stringKey", "hello world!")
                .put("nullKey", null);
            final JSONObject jsonObject2 = new JSONObject();

            jsonObject1.addUpdateListenerGlobal(evt -> {
                final Object oldValue = evt.getOldValue();
                final Object newValue = evt.getNewValue();

                assertNotEquals(oldValue.toString(), newValue.toString());
            });

            jsonObject1.updateOrRemove(jsonObject2);
        } catch (Exception ex) {
            ex.printStackTrace();
            fail(ex.getMessage());
        }
    }

    @Test
    public void updateOrRemoveAllEmptyTest() {
        try {
            final JSONObject jsonObject1 = new JSONObject();
            final JSONObject jsonObject2 = new JSONObject();

            jsonObject1.addUpdateListenerGlobal(evt -> {
                final Object oldValue = evt.getOldValue();
                final Object newValue = evt.getNewValue();

                assertNotEquals(oldValue.toString(), newValue.toString());
            });

            jsonObject1.updateOrRemove(jsonObject2);
        } catch (Exception ex) {
            ex.printStackTrace();
            fail(ex.getMessage());
        }
    }

    @Test
    public void updateOrRemoveSrcTest() {
        try {
            final JSONObject jsonObject1 = new JSONObject()
                .put("stringKey", "hello world!");
            final JSONObject jsonObject2 = new JSONObject();

            jsonObject1.addUpdateListenerGlobal(evt -> {
                final Object oldValue = evt.getOldValue();
                final Object newValue = evt.getNewValue();

                assertEquals("{\"stringKey\":\"hello world!\"}", oldValue.toString());
                assertEquals("{}", newValue.toString());
            });

            jsonObject1.addUpdateListener("stringKey", evt -> {
                final Object oldValue = evt.getOldValue();
                final Object newValue = evt.getNewValue();

                assertEquals("hello world!", oldValue);
                assertNull(newValue);
            });

            jsonObject1.updateOrRemove(jsonObject2);
        } catch (Exception ex) {
            ex.printStackTrace();
            fail(ex.getMessage());
        }
    }

    @Test
    public void updateOrRemoveDstTest() {
        try {
            final JSONObject jsonObject1 = new JSONObject();
            final JSONObject jsonObject2 = new JSONObject()
                .put("stringKey", "hello world!");

            jsonObject1.addUpdateListenerGlobal(evt -> {
                final Object oldValue = evt.getOldValue();
                final Object newValue = evt.getNewValue();

                assertEquals("{}", oldValue.toString());
                assertEquals("{\"stringKey\":\"hello world!\"}", newValue.toString());
            });

            jsonObject1.addUpdateListener("stringKey", evt -> {
                final Object oldValue = evt.getOldValue();
                final Object newValue = evt.getNewValue();

                assertNull(oldValue);
                assertEquals("hello world!", newValue);
            });

            jsonObject1.updateOrRemove(jsonObject2);
        } catch (Exception ex) {
            ex.printStackTrace();
            fail(ex.getMessage());
        }
    }

    @Test
    public void newNullTest() {
        try {
            final JSONObject jsonObject1 = new JSONObject();
            final JSONObject jsonObject2 = new JSONObject()
                .put("stringKey", "hello world!");

            jsonObject1.addUpdateListenerGlobal(evt -> {
                final Object oldValue = evt.getOldValue();
                final Object newValue = evt.getNewValue();

                assertEquals("{}", oldValue.toString());
                assertEquals("{\"stringKey\":\"hello world!\"}", newValue.toString());
            });

            jsonObject1.addUpdateListener("stringKey", evt -> {
                final Object oldValue = evt.getOldValue();
                final Object newValue = evt.getNewValue();

                assertNull(oldValue);
                assertEquals("hello world!", newValue);
            });

            jsonObject1.updateOrRemove(jsonObject2);
        } catch (Exception ex) {
            ex.printStackTrace();
            fail(ex.getMessage());
        }
    }
}
