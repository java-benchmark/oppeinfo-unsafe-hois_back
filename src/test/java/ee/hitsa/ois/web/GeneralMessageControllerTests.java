package ee.hitsa.ois.web;

import java.util.Arrays;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.web.util.UriComponentsBuilder;

import ee.hitsa.ois.TestConfigurationService;
import ee.hitsa.ois.enums.Role;
import ee.hitsa.ois.web.commandobject.GeneralMessageForm;
import ee.hitsa.ois.web.dto.GeneralMessageDto;

@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
public class GeneralMessageControllerTests {

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
    public void show() {
        String url = "/generalmessages/show";
        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(url, Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    public void search() {
        String url = "/generalmessages";
        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(url, Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());

        UriComponentsBuilder uriBuilder = UriComponentsBuilder.fromUriString("/generalmessages");
        uriBuilder.queryParam("title", "NIMI");
        uriBuilder.queryParam("content", "3211212");
        uriBuilder.queryParam("validFrom", "2016-12-31T22:00:00.000Z");
        uriBuilder.queryParam("validThru", "2017-01-31T22:00:00.000Z");
        uriBuilder.queryParam("targets", "T1", "T2", "T3");
        url = uriBuilder.build().toUriString();
        responseEntity = restTemplate.getForEntity(url, Object.class);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    public void getMissing() {
        ResponseEntity<Object> responseEntity = restTemplate.getForEntity("/generalmessages/0", Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.NOT_FOUND, responseEntity.getStatusCode());
    }

    @Test
    public void crud() {
        // create
        UriComponentsBuilder uriBuilder = UriComponentsBuilder.fromUriString("/generalmessages");
        String uri = uriBuilder.build().toUriString();
        GeneralMessageForm form = new GeneralMessageForm();
        form.setTitle("Üldteade");
        form.setContent("Üldteate sisu");
        form.setTargets(Arrays.asList(Role.ROLL_P.name()));
        ResponseEntity<GeneralMessageDto> responseEntity = restTemplate.postForEntity(uri, form, GeneralMessageDto.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.CREATED, responseEntity.getStatusCode());
        Assert.assertNotNull(responseEntity.getBody());
        Long id = responseEntity.getBody().getId();
        Assert.assertNotNull(id);

        // read
        uriBuilder = UriComponentsBuilder.fromUriString("/generalmessages").pathSegment(id.toString());
        uri = uriBuilder.build().toUriString();
        ResponseEntity<GeneralMessageDto> response = restTemplate.getForEntity(uri, GeneralMessageDto.class);
        Assert.assertNotNull(response);
        Assert.assertEquals(HttpStatus.OK, response.getStatusCode());

        // update
        form = response.getBody();
        Assert.assertNotNull(form);
        form.setContent("Üldteate uus sisu");
        responseEntity = restTemplate.exchange(uri, HttpMethod.PUT, new HttpEntity<>(form), GeneralMessageDto.class);
        Assert.assertNotNull(response);
        Assert.assertEquals(HttpStatus.OK, response.getStatusCode());

        // read
        responseEntity = restTemplate.getForEntity(uri, GeneralMessageDto.class);
        Assert.assertNotNull(response);
        Assert.assertEquals(HttpStatus.OK, response.getStatusCode());
        Long version = responseEntity.getBody().getVersion();
        Assert.assertNotNull(version);

        // try to update with wrong version
        form.setContent("Üldteate vana sisu");
        responseEntity = restTemplate.exchange(uri, HttpMethod.PUT, new HttpEntity<>(form), GeneralMessageDto.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.CONFLICT, responseEntity.getStatusCode());

        // search existing departments
        UriComponentsBuilder searchUriBuilder = UriComponentsBuilder.fromUriString("/generalmessages");
        ResponseEntity<Object> searchResponseEntity = restTemplate.getForEntity(searchUriBuilder.build().toUriString(), Object.class);
        Assert.assertNotNull(searchResponseEntity);
        Assert.assertEquals(HttpStatus.OK, searchResponseEntity.getStatusCode());

        // delete
        uriBuilder = UriComponentsBuilder.fromUriString("/generalmessages").pathSegment(id.toString());
        uriBuilder.queryParam("version", version);
        uri = uriBuilder.build().toUriString();
        restTemplate.delete(uri);
    }
}
