package ee.hitsa.ois.web;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.web.util.UriComponentsBuilder;

import ee.hitsa.ois.TestConfigurationService;
import ee.hitsa.ois.enums.Role;

@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
public class LogsControllerTests {

    @Autowired
    private TestRestTemplate restTemplate;
    @Autowired
    private TestConfigurationService testConfigurationService;

    @Before
    public void setUp() {
        testConfigurationService.userToRole(Role.ROLL_A, restTemplate);
    }

    @After
    public void cleanUp() {
        testConfigurationService.setSessionCookie(null);
    }

    @Test
    public void ehisLogCurriculum() {
        String url = "/logs/ehis";
        UriComponentsBuilder uriBuilder = UriComponentsBuilder.fromUriString(url);
        uriBuilder.queryParam("messageType", "oisOppekava");
        uriBuilder.queryParam("from", "2017-01-01T00:00:00.000Z");
        uriBuilder.queryParam("thru", "2017-01-01T00:00:00.000Z");
        uriBuilder.queryParam("errors", Boolean.TRUE);

        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(uriBuilder.build().toUriString(), Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    public void ehisLogStudents() {
        String url = "/logs/ehis";
        UriComponentsBuilder uriBuilder = UriComponentsBuilder.fromUriString(url);
        uriBuilder.queryParam("messageType", "laeKorgharidus");
        uriBuilder.queryParam("from", "2017-01-01T00:00:00.000Z");
        uriBuilder.queryParam("thru", "2017-01-01T00:00:00.000Z");
        uriBuilder.queryParam("errors", Boolean.TRUE);

        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(uriBuilder.build().toUriString(), Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    public void ehisLogTeachersHigher() {
        String url = "/logs/ehis";
        UriComponentsBuilder uriBuilder = UriComponentsBuilder.fromUriString(url);
        uriBuilder.queryParam("messageType", "laeOppejoud");
        uriBuilder.queryParam("from", "2017-01-01T00:00:00.000Z");
        uriBuilder.queryParam("thru", "2017-01-01T00:00:00.000Z");
        uriBuilder.queryParam("errors", Boolean.TRUE);
        uriBuilder.queryParam("teacher", Long.valueOf(1));

        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(uriBuilder.build().toUriString(), Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    public void ehisLogTeachersVocational() {
        String url = "/logs/ehis";
        UriComponentsBuilder uriBuilder = UriComponentsBuilder.fromUriString(url);
        uriBuilder.queryParam("messageType", "laePedagoogid");
        uriBuilder.queryParam("from", "2017-01-01T00:00:00.000Z");
        uriBuilder.queryParam("thru", "2017-01-01T00:00:00.000Z");
        uriBuilder.queryParam("errors", Boolean.TRUE);
        uriBuilder.queryParam("teacher", Long.valueOf(1));

        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(uriBuilder.build().toUriString(), Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    public void ehisLogAll() {
        String url = "/logs/ehis";
        UriComponentsBuilder uriBuilder = UriComponentsBuilder.fromUriString(url);
        uriBuilder.queryParam("from", "2017-01-01T00:00:00.000Z");
        uriBuilder.queryParam("thru", "2017-01-01T00:00:00.000Z");
        uriBuilder.queryParam("errors", Boolean.TRUE);

        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(uriBuilder.build().toUriString(), Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    public void ehisGet() {
        String url = "/logs/ehis/0?messageType=laeKorgharidus";
        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(url, Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.NOT_FOUND, responseEntity.getStatusCode());
    }

    @Test
    public void ekisLog() {
        String url = "/logs/ekis";
        UriComponentsBuilder uriBuilder = UriComponentsBuilder.fromUriString(url);
        uriBuilder.queryParam("from", "2017-01-01T00:00:00.000Z");
        uriBuilder.queryParam("thru", "2017-01-01T00:00:00.000Z");
        uriBuilder.queryParam("errors", Boolean.TRUE);

        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(uriBuilder.build().toUriString(), Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    public void ekisGet() {
        String url = "/logs/ekis/0?messageType=registerDirective";
        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(url, Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.NOT_FOUND, responseEntity.getStatusCode());
    }

    @Test
    public void kutseregisterLog() {
        String url = "/logs/kutseregister";
        UriComponentsBuilder uriBuilder = UriComponentsBuilder.fromUriString(url);
        uriBuilder.queryParam("from", "2017-01-01T00:00:00.000Z");
        uriBuilder.queryParam("thru", "2017-01-01T00:00:00.000Z");
        uriBuilder.queryParam("errors", Boolean.TRUE);

        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(uriBuilder.build().toUriString(), Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    public void kutseregisterGet() {
        String url = "/logs/kutseregister/0?messageType=kutseregister.kutsetunnistus";
        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(url, Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.NOT_FOUND, responseEntity.getStatusCode());
    }

    @Test
    public void rtipLog() {
        String url = "/logs/rtip";
        UriComponentsBuilder uriBuilder = UriComponentsBuilder.fromUriString(url);
        uriBuilder.queryParam("from", "2017-01-01T00:00:00.000Z");
        uriBuilder.queryParam("thru", "2017-01-01T00:00:00.000Z");
        uriBuilder.queryParam("errors", Boolean.TRUE);

        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(uriBuilder.build().toUriString(), Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    public void rtipGet() {
        String url = "/logs/rtip/0?messageType=sap.tootajaPohiandmed";
        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(url, Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.NOT_FOUND, responseEntity.getStatusCode());
    }

    @Test
    public void saisLog() {
        String url = "/logs/sais";
        UriComponentsBuilder uriBuilder = UriComponentsBuilder.fromUriString(url);
        uriBuilder.queryParam("from", "2017-01-01T00:00:00.000Z");
        uriBuilder.queryParam("thru", "2017-01-01T00:00:00.000Z");
        uriBuilder.queryParam("errors", Boolean.TRUE);

        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(uriBuilder.build().toUriString(), Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    public void saisGet() {
        String url = "/logs/sais/0?messageType=sais2.AllAdmissionsExport";
        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(url, Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.NOT_FOUND, responseEntity.getStatusCode());
    }
}
