package ee.hitsa.ois.service;

import javax.persistence.EntityManager;
import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import ee.hitsa.ois.domain.enterprise.Enterprise;
import ee.hitsa.ois.service.security.HoisUserDetails;
import ee.hitsa.ois.util.EntityUtil;
import ee.hitsa.ois.web.commandobject.EnterpriseForm;
import ee.hitsa.ois.web.dto.enterprise.EnterpriseDto;

@Transactional
@Service
public class EnterpriseService {

    @Autowired
    private EntityManager em;

    public EnterpriseDto get(Enterprise enterprise) {
        return EnterpriseDto.of(enterprise);
    }

    public Enterprise create(EnterpriseForm enterpriseForm) {
        Enterprise enterprise = EntityUtil.bindToEntity(enterpriseForm, new Enterprise());
        return EntityUtil.save(enterprise, em);
    }

    public void delete(HoisUserDetails user, Enterprise enterprise) {
        EntityUtil.setUsername(user.getUsername(), em);
        EntityUtil.deleteEntity(enterprise, em);
    }
}
