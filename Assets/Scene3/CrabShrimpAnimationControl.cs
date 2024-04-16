using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CrabShrimpAnimationControl : MonoBehaviour
{
    private float timer;
    [SerializeField]private float DelayTime = 0.2f;
    private Animator anim;
    void Start()
    {
        anim = GetComponent<Animator>();
        if (anim)
        {
            anim.enabled = false;
            int randomInt = Random.Range(1, 20);
            DelayTime = (float)randomInt/10;
        }
    }

    // Update is called once per frame
    void Update()
    {
        timer += Time.deltaTime;
        if (timer >= DelayTime)
        {
            timer = 0f;
            if (anim)
            {
                anim.enabled = true;
            }
        }
    }
}
