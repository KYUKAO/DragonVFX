using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class AttachEffectToBone : MonoBehaviour
{
    private ParticleSystem _particleSystem;
    private ParticleSystem.ShapeModule _shapeModule;

    void Start()
    {
        // 获取粒子系统组件
        _particleSystem = GetComponent<ParticleSystem>();
        // 获取粒子系统的Shape模块
        _shapeModule = _particleSystem.shape;
    }

    void Update()
    {
        // 更新Shape模块的Position属性以匹配GameObject的Transform位置
        _shapeModule.position = transform.localPosition;
    }
}
