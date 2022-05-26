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
import ee.hitsa.ois.enums.ProtocolStatus;
import ee.hitsa.ois.enums.ProtocolType;
import ee.hitsa.ois.enums.Role;

@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
public class HigherProtocolControllerTest {
    
    private static final String ENDPOINT = "/higherProtocols";
    private static final Long SUBJECT_STUDY_PERIOD_ID = Long.valueOf(763L);

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
    public void search() {
        UriComponentsBuilder uriBuilder = UriComponentsBuilder.fromUriString(ENDPOINT);
        uriBuilder.queryParam("protocolType", ProtocolType.PROTOKOLLI_LIIK_P.name());
        uriBuilder.queryParam("studyPeriod", Integer.valueOf(1));
        uriBuilder.queryParam("teacher", Integer.valueOf(1));
        uriBuilder.queryParam("subject", "012");
        uriBuilder.queryParam("status", ProtocolStatus.PROTOKOLL_STAATUS_S.name());
        uriBuilder.queryParam("protocolNr", "123");
        uriBuilder.queryParam("confirmDateFrom", "2016-12-31T22:00:00.000Z");
        uriBuilder.queryParam("confirmDateThru", "2016-12-31T22:00:00.000Z");
        uriBuilder.queryParam("insertedFrom", "2016-12-31T22:00:00.000Z");
        uriBuilder.queryParam("insertedThru", "2016-12-31T22:00:00.000Z");
        String url = uriBuilder.build().toUriString();
        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(url, Object.class);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode()); 
    }
    
    @Test
    public void getSubjectStudyPeriods() {
        String url = ENDPOINT + "/subjectStudyPeriods";
        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(url, Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }
    
    @Test
    public void getStudentsForBasicProtocol() {
        getStudents(ProtocolType.PROTOKOLLI_LIIK_P);
    }
    
    @Test
    public void getStudentsForRepeatingProtocol() {
        getStudents(ProtocolType.PROTOKOLLI_LIIK_K);
    }
    
    public void getStudents(ProtocolType protocolType) {
        String url = ENDPOINT + "/students";

        UriComponentsBuilder uriBuilder = UriComponentsBuilder.fromUriString(url);
        uriBuilder.queryParam("protocolType", protocolType.name());
        uriBuilder.queryParam("subjectStudyPeriod", SUBJECT_STUDY_PERIOD_ID);
        url = uriBuilder.build().toUriString();
        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(url, Object.class);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode()); 
    }
}
