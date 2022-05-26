package ee.hitsa.ois.web;

import java.util.Collections;

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
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.web.util.UriComponentsBuilder;

import ee.hitsa.ois.TestConfigurationService;
import ee.hitsa.ois.enums.Role;

@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
@ActiveProfiles("test")
public class SaisClassifierControllerTests {
    
    @Autowired
    private TestRestTemplate restTemplate;
    @Autowired
    private TestConfigurationService testConfigurationService;

    @Before
    public void setUp() {
        testConfigurationService.userToRole(Role.ROLL_P, restTemplate);
    }

    @After
    public void cleanUp() {
        testConfigurationService.setSessionCookie(null);
    }

    @Test
    public void search() {
        UriComponentsBuilder uri = UriComponentsBuilder.fromUriString("/saisClassifier/asdasdasd");
        uri.queryParam("value", "value");
        uri.queryParam("name", "Name");
        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(uri.build().toUriString(), Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    public void list() {
        String uri = UriComponentsBuilder.fromUriString("/saisClassifier").build().toUriString();
        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(uri, Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    public void saisImport() {
        String uri = UriComponentsBuilder.fromUriString("/saisClassifier/saisImport").build().toUriString();
        ResponseEntity<Object> responseEntity = restTemplate.postForEntity(uri, Collections.emptyMap(), Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }
}
