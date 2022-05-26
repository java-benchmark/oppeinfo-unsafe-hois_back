package ee.hitsa.ois.web;

import java.util.HashMap;

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
import ee.hitsa.ois.domain.Classifier;
import ee.hitsa.ois.enums.Role;
import ee.hitsa.ois.service.ClassifierService;
import ee.hitsa.ois.web.dto.ClassifierDto;

@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
public class ClassifierControllerTests {

	private static final String CLASSIFIER_CODE = "ClassifierControllerTests";

	@Autowired
    private TestRestTemplate restTemplate;
	@Autowired
    private ClassifierService classifierService;
	@Autowired
    private TestConfigurationService testConfigurationService;

	@Before
    public void setUp() {
		Classifier classifier = getNewClassifierByCode(CLASSIFIER_CODE);
		classifierService.save(classifier);
		testConfigurationService.userToRole(Role.ROLL_P, restTemplate);
    }

	@After
    public void cleanUp() {
		classifierService.delete(CLASSIFIER_CODE);
		testConfigurationService.setSessionCookie(null);
    }

	@Test
	public void get() {
		ResponseEntity<ClassifierDto> responseEntity = this.restTemplate.getForEntity("/classifier/{code}", ClassifierDto.class, CLASSIFIER_CODE);
		Assert.assertNotNull(responseEntity);
		Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());

		ClassifierDto classifier = responseEntity.getBody();
		Assert.assertNotNull(classifier);
		Assert.assertNotNull(classifier.getCode());
	}

	@Test
	public void createAndDelete() {
		//create
		Classifier classifier = getNewClassifierByCode(CLASSIFIER_CODE + "Create");
		ResponseEntity<ClassifierDto> responseEntity = this.restTemplate.postForEntity("/classifier", classifier, ClassifierDto.class, new HashMap<>());
		Assert.assertNotNull(responseEntity);
		Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());

		ClassifierDto createdClassifier = responseEntity.getBody();
		Assert.assertNotNull(createdClassifier);
		Assert.assertEquals(Long.valueOf(0), createdClassifier.getVersion());
		Assert.assertNotNull(createdClassifier.getInserted());

		//delete
		String uri = "";
        UriComponentsBuilder uriBuilder = UriComponentsBuilder.fromUriString("/classifier").pathSegment(classifier.getCode().toString());
        uriBuilder.queryParam("version", Integer.valueOf(0));
        uri = uriBuilder.build().toUriString();
        restTemplate.delete(uri);
	}

	@Test
	public void update() {
	    ClassifierDto classifier = this.restTemplate.getForEntity("/classifier/{code}", ClassifierDto.class, CLASSIFIER_CODE).getBody();
		classifier.setNameEt("changed");

		ResponseEntity<ClassifierDto> responseEntity =
		        this.restTemplate.exchange("/classifier/{code}", HttpMethod.PUT, new HttpEntity<>(classifier), ClassifierDto.class, CLASSIFIER_CODE);

		Assert.assertNotNull(responseEntity);
		Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());

		ClassifierDto updatedClassifier = responseEntity.getBody();
		Assert.assertNotNull(updatedClassifier);
		Assert.assertEquals(Long.valueOf(1), updatedClassifier.getVersion());
		Assert.assertNotNull(updatedClassifier.getInserted());
		Assert.assertNotNull(updatedClassifier.getChanged());
		Assert.assertNotEquals(updatedClassifier.getInserted(), updatedClassifier.getChanged());
	}

    @Test
    public void testSearch() {
        UriComponentsBuilder uri = UriComponentsBuilder.fromUriString("/classifier");
        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(uri.build().toUriString(), Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());

        uri = UriComponentsBuilder.fromUriString("/classifier");
        uri.queryParam("code", "code");
        uri.queryParam("value", "value");
        responseEntity = restTemplate.getForEntity(uri.build().toUriString(), Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    public void testSearchTables() {
        String uri = UriComponentsBuilder.fromUriString("/classifier/heads").build().toUriString();
        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(uri, Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
	}

	@Test
	public void testGetPossibleParentClassifiers() {
        UriComponentsBuilder uriBuilder = UriComponentsBuilder.fromUriString("/classifier/getPossibleParentClassifiers");
        uriBuilder.queryParam("name", "Eesti");
        uriBuilder.queryParam("lang", "ET");
        uriBuilder.queryParam("mainClassCode", "EKR");

        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(uriBuilder.build().toUriString(), Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());

        uriBuilder = UriComponentsBuilder.fromUriString("/classifier/getPossibleParentClassifiers");
        uriBuilder.queryParam("name", "Estonia");
        uriBuilder.queryParam("lang", "EN");
        uriBuilder.queryParam("mainClassCode", "EKR");

        responseEntity = restTemplate.getForEntity(uriBuilder.build().toUriString(), Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
	}

	@Test
	public void testGetParents() {
    	String uri = UriComponentsBuilder.fromUriString("/classifier/parents/ISCED_SUUN_018").build().toUriString();
        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(uri, Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
	}

	@Test
	public void testGetChildren() {
    	String uri = UriComponentsBuilder.fromUriString("/classifier/children/ISCED_SUUN_018").build().toUriString();
        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(uri, Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
	}

	@Test
	public void testGetParentsByMainClassCode() {
    	String uri = UriComponentsBuilder.fromUriString("/classifier/parents/ISCED_VALD/ISCED_SUUN_018").build().toUriString();
        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(uri, Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
	}

	@Test
	public void testGetPossibleConnections() {
    	String uri = UriComponentsBuilder.fromUriString("/classifier/connections/OPPEASTE").build().toUriString();
        ResponseEntity<Object> responseEntity = restTemplate.getForEntity(uri, Object.class);
        Assert.assertNotNull(responseEntity);
        Assert.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
	}

	private static Classifier getNewClassifierByCode(String code) {
		Classifier newClassifier = new Classifier();
		newClassifier.setCode(code);
		newClassifier.setValue(code);
		newClassifier.setNameEt(code);
		return newClassifier;
	}
}
